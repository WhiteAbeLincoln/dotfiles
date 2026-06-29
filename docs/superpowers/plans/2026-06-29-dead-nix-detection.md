# Dead-`.nix` Detection & Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a committed, reusable detector that finds `.nix` files unreachable from the active flake outputs, then use it to remove the dead files (campbell + the windows tree it dragged in, and anything else orphaned).

**Architecture:** A Python script walks the import graph using Nix's own parser (`nix-instantiate --parse`), seeded from `flake.nix`, and reports every git-tracked `.nix` file not reached. Cleanup prunes the orphaned `unstableOverlay` binding from `flake.nix`, deletes the detected dead files, and proves the removals were inert by asserting each active config's `toplevel.drvPath` is byte-identical before and after.

**Tech Stack:** Python 3.11+ (stdlib only, run via `uv run`), `nix-instantiate`, `git`, `nix eval`/`nix flake check`.

## Global Constraints

- Active flake outputs are exactly two: `nixosConfigurations.globalhawk` and `darwinConfigurations.nighthawk`. The WSL output is retired; do **not** seed the graph from anything campbell-related.
- The **`nixpkgs-unstable` flake input must stay** — `modules/common/overlays.nix:7` uses `inputs.nixpkgs-unstable`. Only the inline `unstableOverlay` `let`-binding and the now-unused `outputs` destructure entry are removed.
- Python: stdlib only, run via `uv run`; PEP 723 inline metadata header on the script. No `pip`/`requirements.txt`.
- This env resolves to **BSD sed** in the shell despite the nix GNU sed — use the Edit tool for file edits, never `sed -i`.
- Commit messages document the **why**, not a file-by-file list of what changed.
- Do not run `switch`/activation. Validation uses `build`/`eval`/`flake check` only.
- Format any new/edited `.nix` with `nix fmt` (alejandra) before committing.
- Script filename uses underscores (`find_dead_nix.py`) so it is importable by the test.

---

### Task 1: Build the detector + hermetic fixture tests

**Files:**
- Create: `misc/find_dead_nix.py`
- Test: `misc/test_find_dead_nix.py`

**Interfaces:**
- Consumes: nothing (first task).
- Produces:
  - `parse_output(file: Path) -> str` — parsed-AST text; raises `ValueError` on parse failure.
  - `nix_targets(file: Path, root: Path) -> set[Path]` — resolved `.nix` files referenced by `file` (dir refs → `default.nix`; non-`.nix` and out-of-`root` ignored).
  - `compute_reachable(roots: set[Path], root: Path, errors: list[str] | None = None) -> dict[Path, Path | None]` — BFS closure; maps each reached file to the parent that pulled it in (roots map to `None`); appends parse failures to `errors` and treats those files as leaves.
  - `find_dead(all_nix: set[Path], reachable: set[Path]) -> set[Path]`.
  - CLI: `main() -> int` — exits non-zero when the dead set is non-empty.

- [ ] **Step 1: Write the failing tests**

Create `misc/test_find_dead_nix.py`:

```python
import textwrap
from pathlib import Path

import find_dead_nix as fdn


def _write(p: Path, body: str) -> None:
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(textwrap.dedent(body))


def test_finds_orphan_and_resolves_transitive_and_dir_imports(tmp_path):
    root = tmp_path
    _write(root / "root.nix", "{ imports = [ ./a ./b.nix ]; }")
    _write(root / "a" / "default.nix", "{ imports = [ ../c.nix ]; }")
    _write(root / "b.nix", "{ x = 1; }")
    _write(root / "c.nix", "{ y = 2; }")
    _write(root / "orphan.nix", "{ z = 3; }")

    all_nix = {
        root / p
        for p in ["root.nix", "a/default.nix", "b.nix", "c.nix", "orphan.nix"]
    }
    parent = fdn.compute_reachable({root / "root.nix"}, root)
    reachable = set(parent)
    dead = fdn.find_dead(all_nix, reachable)

    # directory import resolves to its default.nix
    assert (root / "a" / "default.nix").resolve() in reachable
    # transitive import (root -> a -> c) is reached
    assert (root / "c.nix").resolve() in reachable
    # the only unreachable file is the orphan
    assert dead == {(root / "orphan.nix").resolve()}


def test_nix_targets_ignores_data_files_and_resolves_dirs(tmp_path):
    root = tmp_path
    _write(root / "m.nix", "{ imports = [ ./sub ]; key = ./data.txt; }")
    _write(root / "sub" / "default.nix", "{ }")
    (root / "data.txt").write_text("hello")

    targets = fdn.nix_targets(root / "m.nix", root)

    assert targets == {(root / "sub" / "default.nix").resolve()}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd misc && uv run --with pytest pytest test_find_dead_nix.py -v`
Expected: collection error / FAIL — `ModuleNotFoundError: No module named 'find_dead_nix'` (the script does not exist yet).

- [ ] **Step 3: Write the detector**

Create `misc/find_dead_nix.py`:

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Find .nix files unreachable from the active flake outputs.

Walks the import graph using Nix's own parser (`nix-instantiate --parse`),
seeded from flake.nix, and reports every git-tracked .nix file not reached.
Exit status is non-zero when dead files exist, so it can run as a lint.
"""
from __future__ import annotations

import re
import subprocess
import sys
from collections import deque
from pathlib import Path


def parse_output(file: Path) -> str:
    """Return the parsed-AST text for `file` via `nix-instantiate --parse`."""
    proc = subprocess.run(
        ["nix-instantiate", "--parse", str(file)],
        capture_output=True,
        text=True,
    )
    if proc.returncode != 0:
        raise ValueError(f"parse failed for {file}: {proc.stderr.strip()}")
    return proc.stdout


def nix_targets(file: Path, root: Path) -> set[Path]:
    """`.nix` files referenced by `file`, resolved under `root`.

    A reference to a directory resolves to its `default.nix`. References to
    non-.nix data files, or to paths outside `root`, are ignored. Path nodes
    print unquoted in the parsed AST, so absolute paths under `root` are
    genuine references rather than string mentions.
    """
    out = parse_output(file)
    token = re.compile(re.escape(str(root)) + r"[^\s()\[\]{};,\"]*")
    targets: set[Path] = set()
    for match in token.findall(out):
        p = Path(match)
        if p.is_dir():
            p = p / "default.nix"
        if p.suffix == ".nix" and p.is_file():
            targets.add(p.resolve())
    return targets


def compute_reachable(
    roots: set[Path], root: Path, errors: list[str] | None = None
) -> dict[Path, Path | None]:
    """BFS the import graph from `roots`; map each reached file to its parent."""
    parent: dict[Path, Path | None] = {r.resolve(): None for r in roots}
    queue: deque[Path] = deque(parent)
    while queue:
        cur = queue.popleft()
        try:
            tgts = nix_targets(cur, root)
        except ValueError as exc:
            if errors is not None:
                errors.append(str(exc))
            tgts = set()
        for tgt in tgts:
            if tgt not in parent:
                parent[tgt] = cur
                queue.append(tgt)
    return parent


def find_dead(all_nix: set[Path], reachable: set[Path]) -> set[Path]:
    return {p.resolve() for p in all_nix} - {p.resolve() for p in reachable}


def git_root() -> Path:
    out = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True,
        text=True,
        check=True,
    )
    return Path(out.stdout.strip())


def tracked_nix(root: Path) -> set[Path]:
    out = subprocess.run(
        ["git", "ls-files", "-z", "*.nix"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    )
    return {(root / f).resolve() for f in out.stdout.split("\0") if f}


def cross_check(dead: Path, root: Path) -> list[str]:
    """Grep the repo for references the parser may have missed.

    Searches the repo-relative path and (for default.nix) the parent directory
    name. Bare `default.nix` is non-unique and deliberately skipped.
    """
    rel = dead.relative_to(root)
    needles = [str(rel)]
    if dead.name == "default.nix":
        needles.append(rel.parent.name)
    hits: list[str] = []
    for needle in needles:
        res = subprocess.run(
            ["git", "grep", "-l", "-F", needle],
            cwd=root,
            capture_output=True,
            text=True,
        )
        for line in res.stdout.splitlines():
            if (root / line).resolve() != dead:
                hits.append(f"{needle} -> {line}")
    return hits


def main() -> int:
    verbose = "--verbose" in sys.argv[1:]
    root = git_root()
    errors: list[str] = []
    parent = compute_reachable({root / "flake.nix"}, root, errors)
    reachable = set(parent)
    dead = sorted(find_dead(tracked_nix(root), reachable))

    if verbose:
        print("# reachable (file <- parent)")
        for f in sorted(reachable):
            par = parent[f]
            prel = par.relative_to(root) if par else "(root)"
            print(f"  {f.relative_to(root)}  <-  {prel}")
        print()

    if errors:
        print("# parse errors (treated as leaves):")
        for e in errors:
            print(f"  ! {e}")
        print()

    print(f"# {len(dead)} dead .nix file(s):")
    for d in dead:
        print(d.relative_to(root))
        for hit in cross_check(d, root):
            print(f"    ! cross-check reference: {hit}")
    return 1 if dead else 0


if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd misc && uv run --with pytest pytest test_find_dead_nix.py -v`
Expected: PASS (2 passed).

- [ ] **Step 5: Commit**

```bash
git add misc/find_dead_nix.py misc/test_find_dead_nix.py
git commit -m "feat: Add dead-.nix detector

Cleaning the repo by eye is error-prone; walk the import graph from the
active flake outputs and report unreachable files, kept as a reusable lint."
```

---

### Task 2: Validate the detector against the live repo

This is the human-review gate before any destructive deletion. campbell + the windows tree are known-dead (campbell restored on disk but unreferenced by `flake.nix`), so a correct detector must flag them. Its output is the authoritative dead list consumed by Task 4.

**Files:** none modified (read-only validation).

**Interfaces:**
- Consumes: `misc/find_dead_nix.py` from Task 1.
- Produces: the confirmed dead-file list (recorded for Task 4).

- [ ] **Step 1: Run the detector against the repo**

Run: `uv run misc/find_dead_nix.py --verbose`
Expected: a `# N dead .nix file(s):` block.

- [ ] **Step 2: Confirm the known-dead subtree is flagged**

The dead list MUST contain all of:
```
machine/campbell/home.nix
modules/windows/default.nix
modules/windows/winenv/default.nix
modules/windows/winenv/makeHashmap.nix
modules/windows/winfiles/default.nix
modules/windows/winget/default.nix
simple-darwin.nix
de/kde-xmonad/default.nix
```
If any is missing, STOP — the detector is wrong; debug `nix_targets`/`compute_reachable` before proceeding.

- [ ] **Step 3: Confirm active files are NOT flagged**

The dead list MUST NOT contain any of these (spot-check that live code survives):
```
flake.nix
modules/common/default.nix
modules/darwin/default.nix
modules/hm/default.nix
machine/nighthawk/home.nix
machine/globalhawk/home.nix
program/zsh/default.nix
role/general.nix
```
If any appears, STOP — the graph walk is dropping a real edge; debug before proceeding.

- [ ] **Step 4: Review any additional flagged files**

The detector may surface further orphans (e.g. programs only campbell imported). For each additional entry, read the file briefly and check the `! cross-check reference:` lines under it. A clean entry (no cross-check hits, no surprising consumer) is a genuine deletion candidate. Record the full confirmed list — it is the input to Task 4. No commit (read-only).

---

### Task 3: Prune the orphaned overlay from `flake.nix` and capture the baseline

**Files:**
- Modify: `flake.nix` (remove the `unstableOverlay` `let`-binding, ~lines 88–96; remove `nixpkgs-unstable` from the `outputs` destructure, ~line 36).

**Interfaces:**
- Consumes: nothing from prior tasks (independent edit).
- Produces: baseline drvPath files in the scratchpad, consumed by Task 4.

- [ ] **Step 1: Remove the `unstableOverlay` `let`-binding**

In `flake.nix`, delete this block (campbell was its only consumer; the comment goes with it):

```nix
        # Same overlay as modules/common/overlays.nix, applied at pkgs-
        # construction time for the standalone home-manager config (which
        # doesn't accept nixpkgs.overlays as a module option).
        unstableOverlay = _final: prev: {
          unstable = import nixpkgs-unstable {
            inherit (prev.stdenv.hostPlatform) system;
            config.allowUnfree = true;
          };
        };
```

- [ ] **Step 2: Drop the now-unused `nixpkgs-unstable` from the `outputs` destructure**

In the `outputs = inputs @ { ... }` argument list, remove the `nixpkgs-unstable,` line. The **input declaration on line 9 stays** (`modules/common/overlays.nix` reads `inputs.nixpkgs-unstable`). Confirm nothing else in `flake.nix` references the bare `nixpkgs-unstable` name:

Run: `grep -n "nixpkgs-unstable" flake.nix`
Expected: exactly one hit — the input URL on line 9.

- [ ] **Step 3: Format and verify the flake still evaluates**

```bash
nix fmt flake.nix
nix flake check
```
Expected: `nix flake check` succeeds (the prune is inert — a dead `let`-binding was removed).

- [ ] **Step 4: Capture the baseline drvPaths (flake pruned, files not yet deleted)**

```bash
SP=/private/tmp/claude-501/-Users-abe-dotfiles/50f6bdeb-02f3-479b-af59-fd0664823675/scratchpad
nix eval --raw .#darwinConfigurations.nighthawk.config.system.build.toplevel.drvPath > "$SP/nighthawk.before"
nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath > "$SP/globalhawk.before" \
  || echo "GLOBALHAWK_EVAL_FAILED" > "$SP/globalhawk.before"
cat "$SP/nighthawk.before"; echo; cat "$SP/globalhawk.before"
```
Expected: a `.drv` path for nighthawk. globalhawk is an x86_64-linux config evaluated from aarch64-darwin; if it cannot eval (writes `GLOBALHAWK_EVAL_FAILED`), that is acceptable — Task 4 falls back to `nix flake check` for that output.

- [ ] **Step 5: Commit**

```bash
git add flake.nix
git commit -m "refactor: Drop campbell-only unstableOverlay from flake

The WSL home-manager output was its only consumer; the nixpkgs-unstable
input stays for the system overlay in modules/common/overlays.nix."
```

---

### Task 4: Delete the dead files and prove the removal is inert

**Files:**
- Delete: every file from Task 2's confirmed dead list. Known-expected removals (extend with any additional entries Task 2 surfaced):
  - `machine/campbell/` (whole dir)
  - `modules/windows/` (whole tree)
  - `de/` (whole dir — only `kde-xmonad` + its `files/`)
  - `simple-darwin.nix`

**Interfaces:**
- Consumes: Task 2's dead list; baseline drvPath files from Task 3.
- Produces: nothing downstream (final task).

- [ ] **Step 1: Remove the dead files**

`git rm` the confirmed dead set. For wholly-dead directories remove the directory (this also takes sibling data payloads such as `de/kde-xmonad/files/` and `machine/campbell/`'s contents):

```bash
git rm -r machine/campbell modules/windows de
git rm simple-darwin.nix
# ...plus any additional individual files Task 2 confirmed...
```

- [ ] **Step 2: Re-run the detector — it must now report zero dead files**

Run: `uv run misc/find_dead_nix.py`
Expected: `# 0 dead .nix file(s):` (the dead set was the complement of the reachable closure, so one deletion pass is sufficient — no second-order orphans remain). If non-zero, review the remainder against Task 2 Step 4 and remove genuine orphans.

- [ ] **Step 3: Assert the active configs' drvPaths are unchanged**

```bash
SP=/private/tmp/claude-501/-Users-abe-dotfiles/50f6bdeb-02f3-479b-af59-fd0664823675/scratchpad
NIGHTHAWK_AFTER=$(nix eval --raw .#darwinConfigurations.nighthawk.config.system.build.toplevel.drvPath)
diff <(cat "$SP/nighthawk.before") <(printf '%s' "$NIGHTHAWK_AFTER") \
  && echo "NIGHTHAWK DRVPATH UNCHANGED" || echo "NIGHTHAWK DRVPATH CHANGED — INVESTIGATE"
if ! grep -q GLOBALHAWK_EVAL_FAILED "$SP/globalhawk.before"; then
  GLOBALHAWK_AFTER=$(nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath)
  diff <(cat "$SP/globalhawk.before") <(printf '%s' "$GLOBALHAWK_AFTER") \
    && echo "GLOBALHAWK DRVPATH UNCHANGED" || echo "GLOBALHAWK DRVPATH CHANGED — INVESTIGATE"
fi
```
Expected: `... DRVPATH UNCHANGED` for nighthawk (and globalhawk if it evaluated). A CHANGED result means a removed file was actually live — restore it (`git checkout HEAD -- <path>`) and investigate why the graph missed the edge before continuing.

- [ ] **Step 4: Final full check**

Run: `nix flake check`
Expected: success — all (now two) outputs evaluate.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "chore: Remove .nix files unreachable from active configs

Retiring the WSL output orphaned machine/campbell and the modules/windows
tree; simple-darwin.nix and de/kde-xmonad were already unreferenced. Removal
verified inert: each active config's toplevel.drvPath is unchanged."
```

---

## Notes for the implementer

- The detector deliberately uses `git ls-files` to enumerate `.nix` files, so untracked scratch files are ignored. Ensure the dead files being validated are tracked (campbell was restored to a tracked state).
- A false *positive* in edge extraction (a repo-root path appearing inside a string literal) only keeps a file alive — it can cause under-deletion, never wrongful deletion. The drvPath check and `nix flake check` are the backstops against the dangerous direction (wrongful deletion).
- Do not delete files in Task 4 that Task 2 did not confirm. The plan lists the known-expected set; the detector's actual output is authoritative.

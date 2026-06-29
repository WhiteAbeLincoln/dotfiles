# Dead-`.nix` detection & cleanup — design

**Status:** design, awaiting review
**Date:** 2026-06-29

## Goal

Remove every `.nix` file in this repo that is not reachable from an **active**
flake output, and do it *programmatically* — by computing the import graph —
rather than by eyeballing source files. Ship the detector as a committed,
reusable lint so dead-file creep can be caught again later.

"Active outputs" are now exactly two:

- `nixosConfigurations.globalhawk` (x86_64-linux)
- `darwinConfigurations.nighthawk` (aarch64-darwin)

The standalone WSL home-manager output (`homeConfigurations."awhite@4ZTHR73"`,
campbell) is being **retired entirely**. Its `flake.nix` block has already been
removed. `machine/campbell/` has been restored on disk but is now unreferenced,
which makes it (and the `./modules/windows` tree it was the sole importer of) a
known-dead subtree — a built-in test case for the detector.

## Why static + eval, not syscall tracing

A scan of the repo found:

- **No computed imports of repo `.nix` modules.** Imports are explicit
  `imports = [ ./path ]` / `import ./path` lists. The only dynamic paths are
  external fetches (`packages/nur/nur.nix`), nixpkgs-internal (`modulesPath` in
  `hardware-configuration.nix`), or *data*-file reads (`builtins.path` in
  `program/plex`, `builtins.readDir`/`readFile` of the ai-agents context dir).
  None pull in repo `.nix` modules.
- **No `isWSL`/platform-gated module imports.** Nothing imports a repo `.nix`
  file behind a condition that is now always-false.

So for this repo the **static import graph equals the evaluation import graph**
for `.nix` files: there is nothing "referenced but eval-dead" to miss. A static
walk is exact here, and full evaluation is the correctness backstop. Syscall
tracing (`strace`/`dtruss`/`fs_usage`) is rejected: painful on macOS, requires
cross-evaluating the Linux config from darwin, captures nixpkgs store paths that
must be filtered, and is *more* aggressive (drops eval-inactive branches) than we
want for a first pass.

## Parser choice

Use Nix's own parser via `nix-instantiate --parse <file>`:

- Zero install (ships with Nix), uses the real grammar — no hand-written parser.
- Resolves relative path literals to **absolute** paths.
- Renders path nodes unquoted `( /abs/path )` while string literals stay quoted,
  so extracting genuine path *references* (not string mentions or comments) is a
  robust grep for absolute paths under the repo root.

Verified against `flake.nix` and `role/general.nix` during design. No mainstream
turnkey tool builds a *source* import graph (`deadnix` finds dead bindings within
a file; `statix` lints; `nix-tree`/`nix-du`/`nix-visualize` graph *runtime store
closures*). `tree-sitter-nix` is a viable alternative front-end but needs grammar
compilation + queries for no gain here.

## Component 1 — the detector (`misc/find-dead-nix.py`)

A single-file Python script, stdlib-only, run via `uv run` (PEP 723 inline
metadata header even though it has no third-party deps), committed for reuse.

Algorithm:

1. **Root:** `flake.nix` (it is the entry point; nothing imports it). `flake.nix`
   is always considered live.
2. **Edge extraction:** for each live file, run `nix-instantiate --parse` and
   collect absolute paths under the repo root from its output. Resolve a path
   that names a directory to `<dir>/default.nix`. Follow only targets that are
   `.nix` files (record non-`.nix` data references but do not traverse them).
3. **Reachable set:** BFS closure from the root.
4. **Dead set:** `{all repo .nix files}` − `{reachable}`. (Repo `.nix` files are
   enumerated with `git ls-files '*.nix'` so untracked scratch files and the
   `.git` dir are excluded.)
5. **Cross-check:** for each dead candidate, grep the whole repo for references
   the parser could not resolve — its repo-relative path and its parent directory
   name (e.g. `modules/windows` and `winenv`/`winfiles`), **not** bare
   `default.nix` (non-unique). Defense-in-depth; we expect no hits. Flagged
   candidates are reported, not auto-trusted.
6. **Output:** print the dead list (the actionable result) and, behind a
   `--verbose` flag, the reachable set with the parent that pulled each file in
   (so a surprising "kept" file can be explained).

Exit non-zero when the dead set is non-empty, so it can serve as a CI/pre-commit
lint later.

Edge cases handled explicitly:

- A path reference to a directory with no `default.nix` → report as a dangling
  reference rather than silently dropping it.
- `nix-instantiate --parse` failure on a file (syntax error, IFD) → report the
  file as unparseable rather than treating it as a leaf.

## Component 2 — the cleanup

Sequencing (prune the flake first so the detector's seed is honest):

1. **`flake.nix`:** remove the now-orphaned `unstableOverlay` `let`-binding
   (lines ~88–96; campbell was its only consumer) and drop `nixpkgs-unstable`
   from the `outputs` destructure if it becomes unused there. The
   **`nixpkgs-unstable` input stays** — `modules/common/overlays.nix:7` still
   uses `inputs.nixpkgs-unstable`.
2. **Run the detector** against the pruned flake → dead list. Expected members:
   `machine/campbell/home.nix`, the `modules/windows/*` tree, `simple-darwin.nix`,
   `de/kde-xmonad/default.nix`, plus anything else the graph surfaces.
3. **`git rm`** the dead files. When an entire module directory is dead, remove
   the directory (including its `files/` payloads and sibling data files).

## Component 3 — validation (ground-truth backstop)

Stronger than "it still builds": the active configs' derivations must be
**byte-identical** before and after, proving the removed files were inert.

1. *Before* any change, capture baseline drvPaths:
   ```sh
   nix eval --raw .#darwinConfigurations.nighthawk.config.system.build.toplevel.drvPath
   nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath
   ```
   (Baseline must be taken on a tree where `flake.nix` already evaluates — i.e.
   after step 1's flake pruning but before file deletion, so the comparison
   isolates the file removals.)
2. *After* deletion, re-eval and assert each drvPath is unchanged.
3. Final `nix flake check`.

Caveat: evaluating the x86_64-linux globalhawk `.drvPath` from aarch64-darwin is
normally fine (pure eval). If it hits a platform/IFD snag, fall back to
`nix flake check` for that output and note it; the nighthawk drvPath comparison
still gives a native ground-truth check.

## Non-goals

- Not removing dead *bindings within* files (that is `deadnix`'s job, separate).
- Not touching the `nixpkgs-unstable` input or `modules/common/overlays.nix`.
- Not reworking the import structure — only removing unreachable files.
- Not auto-deleting cross-check-flagged candidates without a human look.

## Verification

```sh
uv run misc/find-dead-nix.py            # lists dead files; flags campbell+windows
# ...apply flake pruning + git rm...
nix flake check                          # eval all (now two) outputs
# drvPath before == after for both active configs
```

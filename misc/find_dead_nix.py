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


def cross_check(candidate: Path, root: Path) -> list[str]:
    """Grep the repo for references the parser may have missed.

    Searches the repo-relative path and (for default.nix) the parent directory's
    repo-relative path (e.g. "modules/windows"). Using the full path fragment
    avoids false positives from bare English words that happen to match a
    directory name (e.g. "windows" in tmux docs). Bare `default.nix` is
    non-unique and deliberately skipped.
    """
    rel = candidate.relative_to(root)
    needles = [str(rel)]
    if candidate.name == "default.nix":
        needles.append(str(rel.parent))
    hits: list[str] = []
    for needle in needles:
        res = subprocess.run(
            ["git", "grep", "-l", "-F", needle],
            cwd=root,
            capture_output=True,
            text=True,
        )
        for line in res.stdout.splitlines():
            if (root / line).resolve() != candidate:
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

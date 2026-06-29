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

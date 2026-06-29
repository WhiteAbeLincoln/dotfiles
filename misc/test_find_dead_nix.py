import subprocess
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


def _git_init_and_add(root: Path) -> None:
    """Initialise a bare git repo and stage all files so `git grep` works."""
    subprocess.run(["git", "init", "-q"], cwd=root, check=True)
    subprocess.run(
        ["git", "-c", "user.email=t@t", "-c", "user.name=t",
         "commit", "--allow-empty", "-m", "init"],
        cwd=root, check=True,
    )
    subprocess.run(["git", "add", "-A"], cwd=root, check=True)


def test_cross_check_no_false_positive_on_bare_word(tmp_path):
    """cross_check must not flag files that only mention the bare dir name.

    Using the full path fragment "modules/windows" prevents a file that
    simply comments on "windows" (the OS) from appearing as a hit.
    """
    root = tmp_path
    # The dead candidate
    _write(root / "modules" / "windows" / "default.nix", "{ }")
    # Unrelated file that mentions the bare word "windows" in a comment
    _write(root / "program" / "tmux" / "module.nix",
           "# winget is a windows package manager\n{ }")
    # A file that genuinely references the path – should be reported as a hit
    _write(root / "role" / "general.nix",
           "{ imports = [ ../modules/windows ]; }")

    _git_init_and_add(root)

    candidate = (root / "modules" / "windows" / "default.nix").resolve()
    hits = fdn.cross_check(candidate, root)

    # The tmux file must not appear in hits (it only has the bare word)
    hit_files = {h.split(" -> ")[-1] for h in hits}
    assert "program/tmux/module.nix" not in hit_files

    # The role file which references the path fragment SHOULD be reported
    assert any("role/general.nix" in h for h in hits)


def test_compute_reachable_tolerates_parse_errors(tmp_path):
    """compute_reachable must record broken files in errors and continue.

    A parse failure on one imported file must not prevent other reachable
    files from being discovered.
    """
    root = tmp_path
    # Root imports both a broken file and a valid sibling
    _write(root / "root.nix",
           "{ imports = [ ./broken.nix ./valid.nix ]; }")
    # Syntactically broken – nix-instantiate --parse will fail
    _write(root / "broken.nix", "{ broken =")
    # Valid sibling
    _write(root / "valid.nix", "{ x = 1; }")

    errors: list[str] = []
    parent = fdn.compute_reachable({root / "root.nix"}, root, errors)
    reachable = set(parent)

    # The broken file should have been recorded as an error
    assert any("broken.nix" in e for e in errors)
    # The valid sibling must still be reached
    assert (root / "valid.nix").resolve() in reachable

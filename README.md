# dotfiles

This repository uses [nix](https://nixos.org) and [home-manager](https://github.com/nix-community/home-manager) to manage dotfiles.

## Installing

Clone this repository into `~/.config/nixpkgs` (on Linux) or `~/.nixpkgs` (on Darwin). Create a `home.nix` or `darwin-configuration.nix` file and import the correct module
from `machine`

## Encryption

Some proprietary firmware is required for linux to work properly on a t2 macbook. Broadcom licensing
prohibits redistribution of the wifi and bluetooth firmware. I've encrypted the firmware package to
avoid any legal issues from storing firmware ripped from my macbook in a publicly accessible git repo.
These files are encrypted with git-crypt. To decrypt, run `gpg --decrypt local.key.asc | git-crypt unlock -`.

## Known Issues
Sometimes on macos in zsh, darwin-rebuild will fail with an error due to the
NIX_PATH. This is most likely because nix-darwin sets up the NIX_PATH in
/etc/zshenv, but the nix-daemon script overwrites this in /etc/zshrc,
which is loaded later.
To fix, replace the line in /etc/zshrc that loads the daemon with:

```sh
# this will overwrite the existing NIX_PATH set by nix-darwin in /etc/zshenv
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  OLD_NIX_PATH="$NIX_PATH"
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  if [ -n "$OLD_NIX_PATH" ]; then
      NIX_PATH="$OLD_NIX_PATH"
  fi
fi
```

This should be safe since /etc/zshrc is not managed by nix and is only modified
once on install.
Alternatively, set `programs.zsh.shellInit` to `export OLD_NIX_PATH="$NIX_PATH"`,
and `programs.zsh.interactiveShellInit` to:

```sh
if [ -n "$OLD_NIX_PATH" ]; then
  NIX_PATH="$OLD_NIX_PATH"
  unset OLD_NIX_PATH
  echo Reset NIX_PATH
fi
```

This will work because zsh.shellInit is run in /etc/zshenv after nix-darwin sets up
NIX_PATH, and zsh.interactiveShellInit is run in /etc/static/zshrc, which is executed
after the daemon script resets NIX_PATH.

## Maintenance

### Finding dead `.nix` files

`misc/find_dead_nix.py` reports every git-tracked `.nix` file that is **not**
reachable from the active flake outputs, so cruft can be removed without
guessing. It walks the import graph using Nix's own parser
(`nix-instantiate --parse`), seeded from `flake.nix`, and follows every
`import`/`imports = [ ./… ]` path reference transitively.

```sh
uv run misc/find_dead_nix.py            # list dead files (exit 1 if any, 0 if none)
uv run misc/find_dead_nix.py --verbose  # also print the reachable set + why each file is kept
uv run misc/find_dead_nix.py > dead.txt # clean, pipeable list (chrome goes to stderr)
```

Output follows the Unix convention: the dead file paths go to **stdout** (one
per line, pipeable), while the count header, `! cross-check reference:`
annotations, parse errors, and the `--verbose` reachable set go to **stderr**.
So a redirect captures a clean list while you still see the annotations on the
terminal.

The cross-check lines flag places elsewhere in the repo that mention a dead
file's path. They are advisory; a hit in `docs/` or in another dead file is not
a live consumer.

Notes:

- **What counts as "active"** is whatever `flake.nix` imports. To change the
  set (e.g. retire or add a host), edit `flake.nix` and re-run — the seed is
  derived from it, not hardcoded.
- The walk is **conservative**: a path that only appears inside a string
  literal is still treated as a reference, so the tool errs toward keeping a
  file, never toward wrongly deleting one.
- It only sees literal path imports. Computed imports
  (`import (./. + "/${x}")`) and non-`.nix` data files pulled in via
  `builtins.path`/`readDir` are out of scope — this repo currently has none of
  the former.
- **Before deleting**, confirm the removal is inert: capture each active
  config's `toplevel.drvPath`, delete, and check it is unchanged, then
  `nix flake check`. A changed drvPath or a failing check means a removed file
  was actually live.

  ```sh
  nix eval --raw .#darwinConfigurations.nighthawk.config.system.build.toplevel.drvPath
  nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath
  ```

Run its tests with `cd misc && uv run --with pytest pytest test_find_dead_nix.py`.
Design rationale lives in `docs/superpowers/specs/2026-06-29-dead-nix-detection-design.md`.

## References & Interesting nix dotfiles
+ https://github.com/solomon-b/nixos-config
# dotfiles

This repository uses [nix](https://nixos.org) and [home-manager](https://github.com/nix-community/home-manager) to manage dotfiles.

## Installing

Clone this repository into `~/.config/nixpkgs` (on Linux) or `~/.nixpkgs` (on Darwin). Create a `home.nix` or `darwin-configuration.nix` file and import the correct module
from `machine`

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

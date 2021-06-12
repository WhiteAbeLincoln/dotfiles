#!/usr/bin/env sh

# Miscellaneous changes that don't fit into a nix definition

# terminfo for tmux
if [ ! -f terminfo.src ]; then
  curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && gunzip terminfo.src.gz
fi
TERMINFO='' /usr/bin/tic -xse tmux-256color terminfo.src

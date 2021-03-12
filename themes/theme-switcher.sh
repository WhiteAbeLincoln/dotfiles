#!/usr/bin/env sh
OSTYPE=$(uname -s)
isdarwin() { [[ $OSTYPE == "Darwin" ]] }
islinux() { [[ $OSTYPE == "Linux" ]] }

if isdarwin; then
  HOMEDIR="$HOME/.nixpkgs"
  DMENU="choose"
else
  HOMEDIR="$HOME/.config/nixpkgs"
  DMENU="rofi -dmenu"
fi

THEMES="$(find "$HOMEDIR/themes" -type d -links 2 ! -empty | sed "s:$HOMEDIR/themes/::")"
SELECTED="$(echo "$THEMES" | $DMENU)"
echo "import ./themes/$SELECTED" > "$HOMEDIR/current-theme.nix"

# apply theme changes
home-manager switch

# signal termite to reload config
kill -USR1 "$(pgrep termite)"

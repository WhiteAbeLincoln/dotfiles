#!/usr/bin/env sh
HOMEDIR="$HOME/.config/nixpkgs"
THEMES="$(find "$HOMEDIR/themes" -type d -links 2 ! -empty | sed "s:$HOMEDIR/themes/::")"
SELECTED="$(echo "$THEMES" | rofi -dmenu)"
echo "import ./themes/$SELECTED" > "$HOMEDIR/current-theme.nix"

# apply theme changes
home-manager switch

# signal termite to reload config
kill -USR1 "$(pgrep termite)"

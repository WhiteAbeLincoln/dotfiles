#!/usr/bin/env bash
# [ $# -ge 1 -a -d "$1" ] && input="$1" || input="-"

ERROR_FILE="/tmp/themeswitcher-error"
OUT_FILE="/tmp/themswitcher-out"

if [[ ! -d "$HOME/.themes" ]]; then
    echo "no themes directory found"
    exit 72 # EX_OSFILE
fi

theme=$(ls $HOME/.themes/ | rofi -dmenu)
input="$HOME/.themes/$theme"

$HOME/bin/switcher.py $input

# restart lemonbar
pkill lemonbar
barrun &

#!/usr/bin/env bash
# [ $# -ge 1 -a -d "$1" ] && input="$1" || input="-"

ERROR_FILE="/tmp/themeswitcher-error"

if [[ ! -d "$HOME/.shell-themes" ]]; then
    echo "no themes directory found" >> $ERROR_FILE
    exit 72 # EX_OSFILE
fi

readarray -t names <<< "$(find "$HOME"/.shell-themes/* -type d)"
declare -A basenames
for i in "${names[@]}"; do
    basenames[$(basename "$i")]=$i
done
theme=$(printf '%s\n' "${!basenames[@]}" | rofi -dmenu)
old_theme=$(cat "$HOME/.local/share/xthemer/current_theme")
if [[ -n "$theme" &&  "$(basename "$old_theme")" != "$theme" ]]; then
    input="${basenames[$theme]}"

    xthemer -q "$input"/colors.* >> $ERROR_FILE

    # restart lemonbar
    # "$HOME/bin/restart_bar"
fi

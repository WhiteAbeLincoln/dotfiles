function battery_charge {
if [[ -d "/sys/class/power_supply/BAT0" ]]; then
    capacity=$(cat /sys/class/power_supply/BAT0/capacity)
    bat_status=$(cat /sys/class/power_supply/BAT0/status)

    if [[ $bat_status == "Charging" ]] || [[ $bat_status == "Full" ]]; then
        symbol=$'\xe2\x9a\xa1'
    elif [[ $bat_status == "Discharging" ]]; then
        symbol=$'\xf0\x9f\x94\x8b'
    else
        symbol='?'
    fi

    color_green='%{[32m%}'
    color_yellow='%{[1;33m%}'
    color_red='%{[31m%}'
    color_reset='%{[00m%}'
    
    if [[ $capacity -gt "66" ]]; then
        color_out=$color_green
    elif [[ $capacity -gt "33" ]]; then
        color_out=$color_yellow
    else
        color_out=$color_red
    fi
        

    echo ${color_out}${symbol} $capacity${color_reset}
fi
}

precmd() { 
    RPROMPT="%(1j.Jobs: %j .)$(battery_charge) %{$fg[magenta]%}[%w %T]%{$reset_color%}"
}

NEWLINE=$'\n'
PROMPT="%{$fg[green]%}%n@%{$fg[green]%}%M %{$fg_no_bold[blue]%}%~${NEWLINE}%# %{$reset_color%}"

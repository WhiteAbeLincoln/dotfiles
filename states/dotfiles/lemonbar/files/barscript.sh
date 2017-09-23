#!/usr/bin/env bash
set -f

function workspaces {
    local temp
    temp=$(wmctrl -d | awk '{ cols=$1","$2","; if (NF==10) print cols $10; else print cols $9 }')
    local i=0
    for elem in $temp; do
        WORKSPACES[$i]=$elem;
        re=',\*,'
        if [[ $elem =~ $re ]]; then
            CURRENT_WS=$elem
        fi
        (( i+=1 ))
    done

    temp=$(wmctrl -l | awk '{ s=""; for (i=4; i<=NF; i++) s=s $i " "; print $2 "," s }' )
    local IFS=$'\n'
    i=0
    for elem in $temp; do
        # WINDOWS[$i]=$elem;
        IFS=',' read -ra idx <<< "$elem"
        IFS=',' read -ra space <<< "${WORKSPACES[${idx[0]}]}"
        ACTIVE_WORKSPACES[${idx[0]}]="${space[2]}"
        (( i+=1 ))
        unset space
    done

    IFS=',' read -ra idx <<< "$CURRENT_WS"
    local colorized="%{F#${XCOLORS[12]}}${idx[2]}%{F#${XCOLORS[foreground]}}"
    if [[ ${ACTIVE_WORKSPACES[${idx[0]}]} ]]; then
        ACTIVE_WORKSPACES=("${ACTIVE_WORKSPACES[@]/"${idx[2]}"/"$colorized"}")
    else
        ACTIVE_WORKSPACES[${idx[0]}]="$colorized"
    fi

    ACTIVE_WINDOW=$(xdotool getwindowfocus getwindowname)

    unset i
    unset re
    unset idx
    unset temp
    unset IFS
    unset colorized
}

function get_workspaces {
    workspaces
    IFS=',' read -ra idx <<< "$CURRENT_WS"
    x=${idx[0]}
    y=$(( (x+1) % 9))
    (( x-=1 ))

    if (( x < 0 )); then
        (( x = 8 ))
    fi

    if [[ ${#ACTIVE_WINDOW} -gt 50 ]]; then
        ACTIVE_WINDOW="${ACTIVE_WINDOW:0:47}..."
    fi

    local str="%{F#${XCOLORS[foreground]}}"
    str+="%{A4:wmctrl -s $y:}%{A5:wmctrl -s $x:}${ACTIVE_WORKSPACES[*]}%{A}%{A}"
    str+=" : %{F#${XCOLORS[4]}}$ACTIVE_WINDOW%{F#${XCOLORS[foreground]}}"
    echo -n "$str"
}

function clock {
    DATE=$(date "+%a %b %d")
    TIME=$(date "+%H:%M")
}

function get_clock {
    clock
    echo -n "%{F#${XCOLORS[4]}}$DATE $TIME%{F#${XCOLORS[foreground]}}"
}

function music {
    CURRENT_SONG=$(playerctl metadata title 2> /dev/null)
    STATUS=$(playerctl status 2> /dev/null)
    # CURRENT_SONG=$(mpc | head -1)
    # SONGTIME=$(mpc -f %time% | grep "\[" | awk '{print $3}')
    # STATUS=$(mpc -f %time% | grep -oe "\[.*\]")

    if [[ "$STATUS" == "Playing" ]]; then
        STATUS=""
    fi

    if [[ -z "$CURRENT_SONG" ]]; then
        CURRENT_SONG="[NONE]"
    fi
}

function get_music {
    music
    local string="$CURRENT_SONG"
    if [[ -z "$STATUS" ]]; then
        echo -en "%{F#${XCOLORS[11]}}\uf04b ${string}%{F#${XCOLORS[foreground]}}"
    else
        echo -en "%{F#${XCOLORS[9]}}\uf04c ${string}%{F#${XCOLORS[foreground]}}"
    fi
}

function weather {
    local date
    date=$(date "+%H_%M")
    if [[ -f "$HOME/.weather/$date" ]]; then
        WEATHER=$(cat "$HOME/.weather/$date")
    else
        set +f
        for elem in "$HOME"/.weather/*; do
            rm "$elem"
        done
        set -f

        WEATHER=$(wget -q -O- http://www.accuweather.com/en/us/logan-ut/84321/weather-forecast/331213 | awk -F[\"\'] '/acm_RecentLocationsCarousel\.push/{print $2": "$16", "$12"°" }' | head -1)
        echo "$WEATHER" > "$HOME/.weather/$date"
    fi
}

function get_weather {
    weather
    echo -en "%{F#${XCOLORS[14]}}$WEATHER%{F#${XCOLORS[foreground]}}"
}

function volume {
    VOLUME=$(ponymix get-volume)
    ponymix is-muted
    STATUS=$?
}

function get_volume {
    volume
    scroll="%{A:ponymix toggle:}"
    end_scroll="%{A}"
    if [[ ! "$STATUS" == "0" ]]; then
        echo -en "%{F#${XCOLORS[11]}}${scroll}\uf028 $VOLUME%${end_scroll}%{F#${XCOLORS[foreground]}}"
    else
        echo -en "%{F#${XCOLORS[9]}}${scroll}\uf026 $VOLUME%${end_scroll}%{F#${XCOLORS[foreground]}}"
    fi
}

function power {
    if [[ -d "/sys/class/power_supply/BAT0" ]]; then
        CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
        BAT_STATUS=$(cat /sys/class/power_supply/BAT0/status)
    fi
}

function get_power {
    power
    color_red="#ff432d"
    color_yellow="#ffc32d"
    color_green="#43a32d"
    color_out=$color_green

    if [[ $BAT_STATUS == "Charging" ]] || [[ $BAT_STATUS == "Full" ]]; then
        bat_symbol="\uf0e7"
    elif [[ $BAT_STATUS == "Discharging" ]]; then
        bat_symbol="\uf241"
    else
        bat_symbol="?"
    fi

    if [[ $CAPACITY -gt "66" ]]; then
        color_out=$color_green
    elif [[ $CAPACITY -gt "33" ]]; then
        color_out=$color_yellow
    else
        color_out=$color_red
    fi

    echo -en "%{F$color_out}$bat_symbol $CAPACITY%{F#${XCOLORS[foreground]}}"
}

function network {
    if [[ -e "/proc/net/wireless" ]]; then
        WIRELESS_STATUS=$(awk '/^w/ {print $3}' < /proc/net/wireless | grep -Eo '[[:digit:]]+')
        WIRELESS_STATUS=$(( WIRELESS_STATUS*100/70 ))
    fi
}

function get_network {
    network
    string=""
    i=0
    while (( WIRELESS_STATUS > 0 )); do
        wifi_stat="$wifi_stat\xe2\x80\xa2"
        (( WIRELESS_STATUS-=25 ))
    done
    echo -en "%{F#${XCOLORS[12]}}$wifi_stat%{F#${XCOLORS[foreground]}}"
}

function set_sleep {
    echo -en "%{F#${XCOLORS[5]}}%{A:systemctl suspend:}\uf011%{A}%{F#${XCOLORS[foreground]}}"
}

while true; do
    # shellcheck source=/home/abe/.bash_colors
    source "$HOME/.bash_colors"
    buff=""
    buff="${buff}%{B#${XCOLORS[background]}}"
    buff="${buff}%{l} $(get_workspaces) "
    buff="${buff}%{c}$(get_clock) "
    buff="${buff}%{r} ║ $(get_volume) ║ $(get_network)"
    if [[ -d "/sys/class/power_supply/BAT0" ]]; then
        buff="${buff} ║ $(get_power)"
    fi
    buff="${buff} %{A:$HOME/bin/show-tray:}ℹ%{A}"
    buff="${buff}%{F-}%{B-}"

    echo "$buff"
    sleep .20;
done
set +f
# vim:syn=sh

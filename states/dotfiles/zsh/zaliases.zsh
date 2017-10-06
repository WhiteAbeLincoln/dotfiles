alias ls='ls --color=auto'
alias ll='ls -lahF'
alias la='ls -A'
alias l='ls -CF'
alias grep='grep --color=always'

alias sleepy='sudo rtcwake -m mem -u -t $(date +%s -d "tomorrow 06:30")'
alias pulsemixer="xterm -e pulsemixer"

eval "$(hub alias -s)"

function volup {
pactl set-sink-volume "@DEFAULT_SINK@" +8%
}

function voldown {
pactl set-sink-volume "@DEFAULT_SINK@" -8%
}

function ls {
    if [[ "$PWD" =~ "Downloads" ]]; then
        /usr/bin/ls --sort=time $@
    else
        /usr/bin/ls $@
    fi
}

function chenv {
    eval $(~/bin/chenv.sh "$@")
}

function to_hex {
    echo -n "$@" | od -A n -t x1 | tr -d ' '
}

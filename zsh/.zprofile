typeset -U path
path=($GEMBIN $GOBIN $path[@])

# Add private bin
if [[ -d "$HOME/bin" ]]; then
    path=("$HOME/bin" $path[@])
fi

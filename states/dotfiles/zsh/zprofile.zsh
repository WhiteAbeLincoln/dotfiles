typeset -U path
path=($GEM_HOME/bin $GOBIN $path[@])

# Add private bin
if [[ -d "$HOME/bin" ]]; then
    path=("$HOME/bin" $path[@])
fi
if [[ -d "$HOME/.local/bin" ]]; then
    path=("$HOME/.local/bin" $path[@])
fi

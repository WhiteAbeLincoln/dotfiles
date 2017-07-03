export GOPATH="$HOME/go"
export GEMBIN="$HOME/.gem/ruby/2.3.0/bin"
export GOBIN="$GOPATH/bin"
export EDITOR=nvim
export JAVA_HOME=/usr/lib/jvm/default
export NVM_DIR="$HOME/.nvm"

if [[ -n "$DESKTOP_SESSION" ]]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

setopt NO_clobber
setopt printexitvalue
setopt interactivecomments


[[ -f ~/.zaliases ]] && . ~/.zaliases

CNF=/usr/share/doc/pkgfile/command-not-found.zsh
[[ -f $CNF ]] && . $CNF

if command -v stack >/dev/null 2>&1; then
	autoload -U +X bashcompinit && bashcompinit
	eval "$(stack --bash-completion-script stack)"
fi

source <(antibody init)
source ~/.zplugins

# vim keybindings
bindkey -v

bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

ZSH_AUTOSUGGEST_CLEAR_WIDGETS=("${(@)ZSH_AUTOSUGGEST_CLEAR_WIDGETS:#(up|down)-line-or-history}")
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(history-substring-search-up history-substring-search-down)

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

eval $(keychain --eval --quiet --systemd id_rsa)
# OPAM configuration
. /home/abe/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

export GEOMETRY_SYMBOL_ROOT="◆"
export GEOMETRY_PROMPT_PLUGINS=(virtualenv docker_machine exec_time jobs git hg)
export PROMPT_GEOMETRY_COLORIZE_SYMBOL=true
export PROMPT_GEOMETRY_COLORIZE_ROOT=true

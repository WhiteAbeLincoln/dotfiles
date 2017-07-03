# Locale Fix
export LC_TIME="en_US.UTF-8"

# Created by newuser for 5.1.1
# The following lines were added by compinstall
eval $(dircolors ~/.dir_colors)
bindkey -v
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix
zstyle ':completion:*' file-sort name
zstyle ':completion:*' format 'Completing: %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd directory
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*' 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' original false
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt 'Errors: %e'
zstyle ':completion:*' substitute 1
zstyle :compinstall filename '/home/abe/.zshrc'

typeset -U fpath
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit
compinit
# End of lines added by compinstall

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=5000
setopt appendhistory extendedglob
# End of lines configured by zsh-newuser-install
setopt PROMPT_SUBST
autoload -U colors && colors

# swap caps and escape
setxkbmap -option caps:swapescape

THEME='bash'

source ~/.zsh/prompts/$THEME.zsh

#ALIASES
[[ -f ~/.zsh_aliases ]] && . ~/.zsh_aliases

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

source /usr/share/doc/pkgfile/command-not-found.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down


# Remove *-line-or-history widgets from list of widgets that clear the autosuggestion to avoid conflict with history-substring-search-* widgets
ZSH_AUTOSUGGEST_CLEAR_WIDGETS=("${(@)ZSH_AUTOSUGGEST_CLEAR_WIDGETS:#(up|down)-line-or-history}")
# Add history-substring-search-* widgets to list of widgets that clear the autosuggestion
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(history-substring-search-up history-substring-search-down)

# If not running interactively, do not do anything
# [[ $- != *i* ]] && return
# [[ -z "$TMUX" ]] && exec tmux


# [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# added by travis gem
[ -f "$HOME"/.travis/travis.sh ] && source "$HOME"/.travis/travis.sh

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/Documents/Programs/google-cloud-sdk/path.zsh.inc" ]; then source "$HOME/Documents/Programs/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/Documents/Programs/google-cloud-sdk/completion.zsh.inc" ]; then source "$HOME/Documents/Programs/google-cloud-sdk/completion.zsh.inc"; fi

if [ -f "$HOME/.zsh-ng-completion" ]; then source "$HOME/.zsh-ng-completion"; fi

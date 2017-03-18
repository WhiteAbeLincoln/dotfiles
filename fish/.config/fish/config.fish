if not functions -q fundle
    eval (curl -sfL https://git.io/fundle-install)
end

### Plugins {{{
fundle plugin 'tuvistavie/fish-nvm'
fundle plugin 'tuvistavie/fish-fastdir'
fundle plugin 'oh-my-fish/plugin-bang-bang'

fundle init
# }}}

# vim:foldmethod=marker:foldlevel=0

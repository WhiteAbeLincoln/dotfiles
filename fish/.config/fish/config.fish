if not functions -q fundle
    eval (curl -sfL https://git.io/fundle-install)
end

### Plugins {{{
fundle plugin 'edc/bass'
fundle plugin 'tuvistavie/fish-fastdir'
fundle plugin 'oh-my-fish/plugin-bang-bang'

fundle init
# }}}

# vim:foldmethod=marker:foldlevel=0

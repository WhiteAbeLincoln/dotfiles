# vim: ft=yaml
install lemonbar:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:lemonbar', ['lemonbar']) }}

include:
  - ..dotfiles.lemonbar

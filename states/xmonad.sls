# vim: ft=yaml
install xmonad:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:xmonad', ['xmonad']) }}

include:
  - ..dotfiles.xmonad

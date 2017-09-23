# vim: ft=yaml
install dunst:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:dunst', ['dunst']) }}

include:
  - ..dotfiles.dunst

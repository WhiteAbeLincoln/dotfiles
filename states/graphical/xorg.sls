# vim: ft=yaml
install xorg:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:xorg', ['xorg']) }}

include:
  - ..dotfiles.xorg

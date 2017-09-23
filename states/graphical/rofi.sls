# vim: ft=yaml
install rofi:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:rofi', ['rofi']) }}

include:
  - ..dotfiles.rofi

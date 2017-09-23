# vim: ft=yaml
install stalonetray:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:stalonetray', ['stalonetray']) }}

include:
  - ..dotfiles.stalonetray

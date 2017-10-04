# vim: ft=yaml
install termite:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:termite', ['termite']) }}

include:
  - ..dotfiles.termite

# vim: ft=yaml
install xthemer:
  pkg.installed:
    - name: python-pip
  pip.installed:
    - require:
      - pkg: python-pip
    - name: {{ salt['pillar.get']('packages:xthemer', 'xthemer') }}

include:
  - ..dotfiles.xthemer

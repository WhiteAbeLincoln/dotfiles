# vim: ft=yaml
install termite:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:termite', ['termite']) }}

create abes termite directory:
  file.directory:
    - name: /home/abe/.config/termite
    - user: abe
    - group: abe

copy abes termite files:
  file.managed:
    - name: /home/abe/.config/termite/config
    - source: salt://dotfiles/termite_config.part
    - user: abe
    - group: abe

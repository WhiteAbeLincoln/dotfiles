# vim:ft=yaml

ensure rofi:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:rofi', ['rofi']) }}

ensure rofi config dir:
  file.directory:
    - name: /home/abe/.config/rofi
    - user: abe
    - group: abe

manage configuration:
  file.managed:
    - name: /home/abe/.config/rofi/config
    - source: salt://dotfiles/rofi.conf
    - user: abe
    - group: abe

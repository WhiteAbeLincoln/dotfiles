# vim: ft=yaml
install xorg:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:xorg', ['xorg']) }}

copy abes XCompose:
  file.managed:
    - name: /home/abe/.XCompose
    - source: salt://dotfiles/xorg/XCompose
    - user: abe
    - group: abe

copy abes xinitrc:
  file.managed:
    - name: /home/abe/.xinitrc
    - source: salt://dotfiles/xorg/xinitrc.sh
    - user: abe
    - group: abe
    - mode: 0744

copy abes Xresources:
  file.managed:
    - name: /home/abe/.Xresources
    - source: salt://dotfiles/xorg/Xresources
    - user: abe
    - group: abe

copy abes xprofile:
  file.managed:
    - name: /home/abe/.xprofile
    - source: salt://dotfiles/xorg/xprofile
    - user: abe
    - group: abe

create abes .Xresources.d:
  file.directory:
    - name: /home/abe/.Xresources.d

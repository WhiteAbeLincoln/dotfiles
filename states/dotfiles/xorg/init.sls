{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy XCompose:
  file.managed:
    - name: {{ home }}/.XCompose
    - source: salt://dotfiles/xorg/files/XCompose
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy xinitrc:
  file.managed:
    - name: {{ home }}/.xinitrc
    - source: salt://dotfiles/xorg/files/xinitrc.sh
    - user: {{ username }}
    - group: {{ group }}
    - mode: 0744

{{username}} copy Xresources:
  file.managed:
    - name: {{ home }}/.Xresources
    - source: salt://dotfiles/xorg/files/Xresources
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy xprofile:
  file.managed:
    - name: {{ home }}/.xprofile
    - source: salt://dotfiles/xorg/files/xprofile
    - user: {{ username }}
    - group: {{ group }}

{{username}} create .Xresources.d:
  file.directory:
    - name: {{ home }}/.Xresources.d

{{username}} copy urxvt:
  file.managed:
    - name: {{ home }}/.Xresources.d/urxvt
    - source: salt://dotfiles/xorg/files/urxvt
    - user: {{ username }}
    - group: {{ group }}


{% endfor %}

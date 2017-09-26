{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy xthemer config:
  file.managed:
    - name: {{ home }}/.config/xthemer/config.yaml
    - source: salt://dotfiles/xthemer/files/config.yaml
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy xthemer templates:
  file.recurse:
    - name: {{ home }}/.config/xthemer/templates
    - source: salt://dotfiles/xthemer/files/templates
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy xthemer themes:
  file.recurse:
    - name: {{ home }}/.shell-themes
    - source: salt://dotfiles/xthemer/files/themes
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy themer script:
  file.managed:
    - name: {{ home }}/bin/themer
    - source: salt://dotfiles/xthemer/files/themer.sh
    - user: {{ username }}
    - group: {{ group }}
    - mode: 0744
{% endfor %}

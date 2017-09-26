{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} create dunst directory:
  file.directory:
    - name: {{ home }}/.config/dunst
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy dunst files:
  file.managed:
    - name: {{ home }}/.config/dunst/dunstrc.part
    - source: salt://dotfiles/dunst/files/dunstrc.part
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

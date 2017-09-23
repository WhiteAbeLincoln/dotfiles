{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy fish files:
  file.managed:
    - name: {{ home }}/.config/dunst/config.fish
    - source: salt://dotfiles/fish/files/config.fish
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

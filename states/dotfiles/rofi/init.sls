{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy rofi files:
  file.managed:
    - name: {{ home }}/.Xresources.d/rofi
    - source: salt://dotfiles/rofi/files/rofi
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

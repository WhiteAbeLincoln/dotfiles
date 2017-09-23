{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{username}} copy stalonetrayrc:
  file.managed:
    - name: {{ home }}/.stalonetrayrc
    - source: salt://dotfiles/stalonetray/files/stalonetrayrc
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} create termite directory:
  file.directory:
    - name: {{ home }}/.config/termite
    - user: {{ username }}
    - group: {{ group }}

{{username}} copy termite files:
  file.managed:
    - name: {{ home }}/.config/termite/config.part
    - source: salt://dotfiles/termite/files/config.part
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy tmux config:
  file.managed:
    - name: {{ home }}/.tmux.conf
    - source: salt://dotfiles/tmux/files/tmux.conf
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

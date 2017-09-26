{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy tmux config:
  file.managed:
    - name: {{ home }}/.tmux.conf
    - source: salt://dotfiles/tmux/files/tmux.conf
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

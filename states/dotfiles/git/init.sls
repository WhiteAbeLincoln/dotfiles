{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy git config files:
  file.recurse:
    - name: {{ home }}/
    - source: salt://dotfiles/git/files
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

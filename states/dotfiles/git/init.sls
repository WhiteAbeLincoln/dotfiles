{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy git config files:
  file.recurse:
    - name: {{ home }}/
    - source: salt://dotfiles/git/files
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

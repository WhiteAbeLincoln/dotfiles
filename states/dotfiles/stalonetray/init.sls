{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{username}} copy stalonetrayrc:
  file.managed:
    - name: {{ home }}/.stalonetrayrc
    - source: salt://dotfiles/stalonetray/files/stalonetrayrc
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

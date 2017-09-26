{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
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

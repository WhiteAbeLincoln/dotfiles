{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy fish files:
  file.managed:
    - name: {{ home }}/.config/dunst/config.fish
    - source: salt://dotfiles/fish/files/config.fish
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

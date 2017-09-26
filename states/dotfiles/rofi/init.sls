{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy rofi files:
  file.managed:
    - name: {{ home }}/.Xresources.d/rofi
    - source: salt://dotfiles/rofi/files/rofi
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy lemonbar barscript:
  file.managed:
    - name: {{ home }}/.barscript
    - source: salt://dotfiles/lemonbar/files/barscript.sh
    - user: {{ username }}
    - group: {{ group }}
    - mode: 0744

{{username}} copy lemonbar helper files:
  file.recurse:
    - name: {{ home }}/bin
    - source: salt://dotfiles/lemonbar/files/bin
    - user: {{ username }}
    - group: {{ group }}
{% endfor %}

{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy xmonad autorun:
  file.managed:
    - name: {{ home }}/bin/xmonad-autorun
    - source: salt://dotfiles/xmonad/files/xmonad-autorun.sh
    - user: {{ username }}
    - group: {{ group }}
    - mode: 0744

{{username}} copy xmonad directory:
  file.recurse:
    - name: {{ home }}/.xmonad
    - source: salt://dotfiles/xmonad/files/xmonad
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

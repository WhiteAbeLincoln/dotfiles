{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy zsh config files:
  file.recurse:
    - name: {{ home }}/
    - source: salt://dotfiles/zsh/files
    - user: {{ username }}
    - group: {{ group }}

{{username}} update antibody bundles:
  cmd.run:
    - name: antibody bundle < {{ home }}/.bundles.txt
    - cwd: {{ home }}
    - runas: {{ username }}
{% endfor %}

{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
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

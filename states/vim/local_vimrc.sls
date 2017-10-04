{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy vim config files:
  file.managed:
    - name: {{ home }}/.vimrc
    - source: salt://vim/files/vimrc.vim
    - user: {{ username }}
    - group: {{ group }}

# {{username}} install vim plugins:
#   cmd.run:
#     - name: >-
#         vim +PlugInstall +qall
#     - unless: {{ home }}/.vim/plugged/
#     - runas: {{ username }}

{% endfor %}

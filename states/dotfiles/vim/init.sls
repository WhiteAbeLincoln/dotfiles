{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy vim config files:
  file.managed:
    - name: {{ home }}/.vimrc
    - source: salt://dotfiles/vim/files/vimrc.vim
    - user: {{ username }}
    - group: {{ group }}

# {{username}} install vim plugins:
#   cmd.run:
#     - name: >-
#         vim +PlugInstall +qall
#     - unless: {{ home }}/.vim/plugged/
#     - runas: {{ username }}

{% endfor %}

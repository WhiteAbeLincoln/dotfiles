{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy neovim files:
  file.managed:
    - name: {{ home }}/.config/nvim/init.vim
    - source: salt://dotfiles/neovim/files/init.vim
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy neovim files:
  file.managed:
    - name: {{ home }}/.config/nvim/init.vim
    - source: salt://dotfiles/neovim/files/init.vim
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

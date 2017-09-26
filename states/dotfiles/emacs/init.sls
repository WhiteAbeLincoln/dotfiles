{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} clone the spacemacs repo:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:git', ['git']) }}
  git.latest:
    - name: >-
        http://github.com/syl20bnr/spacemacs
    - target: {{ home }}/.emacs.d
    - user: {{ username }}

{{username}} copy spacemacs files:
  file.managed:
    - name: {{ home }}/.spacemacs
    - source: salt://dotfiles/emacs/files/spacemacs.el
    - user: {{ username }}
    - group: {{ group }}

{% endfor %}

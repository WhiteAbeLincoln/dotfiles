{% set email = pillar['secrets']['vault']['email'] %}
{% for username, user in salt['pillar.get']('users', {'abe': {}}) %}
{% set group = user.get('prime_group.name', username) %}
{% set home = user.get('home', '/home/' + username)  %}

{{username}} copy mutt config:
  file.managed:
    - name: {{ home }}/.muttrc
    - source: salt://dotfiles/mutt/files/muttrc.jinja
    - template: jinja
    - user: {{ username }}
    - group: {{ group }}
    - context:
        email: {{ email.address }}
        password: {{ email.password }}
        name: {{ email.name }}

{% endfor %}

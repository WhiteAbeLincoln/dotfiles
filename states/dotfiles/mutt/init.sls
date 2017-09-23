{% for user in salt['pillar.get']('users', [{'username': 'abe'}]) %}
{% set username = user.get('username', 'abe') %}
{% set group = user.get('group', 'abe') %}
{% set home = user.get('home', '/home/' + username)  %}
{% set email = pillar['secrets']['vault']['email'] %}

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

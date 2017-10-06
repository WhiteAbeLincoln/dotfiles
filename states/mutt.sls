# vim:ft=yaml
install mutt:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:mutt', ['mutt']) }}

{% set email = pillar['secrets']['vault']['email'] %}

copy abes mutt config:
  file.managed:
    - name: /home/abe/.muttrc
    - source: salt://dotfiles/muttrc.jinja
    - template: jinja
    - user: abe
    - group: abe
    - context:
        email: {{ email.address }}
        password: {{ email.password }}
        name: {{ email.name }}

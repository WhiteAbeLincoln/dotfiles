{% from "vim/map.jinja" import vim with context %}

include:
  - vim

{{ vim.config_root }}/vimrc:
  file.managed:
    - source: salt://vim/files/global/vimrc
    - template: jinja
    - user: root
    - group: {{ vim.group }}
    - mode: 644
    - makedirs: True
    - defaults:
        config_root: {{ vim.config_root }}

{% from "vim/map.jinja" import vim with context %}

remove gvim:
  pkg.removed:
    - name: {{ vim.pkg.graphical }}

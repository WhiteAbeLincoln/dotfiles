{% from "vim/map.jinja" import vim with context %}

uninstall vim:
  pkg.removed:
    - name: {{ vim.pkg.console }}
install gvim:
  - name: {{ vim.pkg.graphical }}

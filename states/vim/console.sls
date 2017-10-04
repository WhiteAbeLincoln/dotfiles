{% from "vim/map.jinja" import vim with context %}

uninstall gvim:
  pkg.removed:
    - name: {{ vim.pkg.graphical }}
install vim:
  pkg.installed:
    - name: {{ vim.pkg.console }}

{% from "vim/map.jinja" import vim with context %}

{% if salt['pillar.get']('vim:console', True) == True %}
uninstall gvim:
  pkg.removed:
    - name: {{ vim.pkg.graphical }}
install vim:
  pkg.installed:
    - name: {{ vim.pkg.console }}
{% else %}
uninstall vim:
  pkg.removed:
    - name: {{ vim.pkg.console }}
install gvim:
  - name: {{ vim.pkg.graphical }}
{% endif %}

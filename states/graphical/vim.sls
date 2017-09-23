uninstall terminal vim:
  pkg.removed:
      - pkgs: {{ salt['pillar.get']('packages:vim:console', ['vim']) }}

install gvim:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:vim:graphical', ['gvim']) }}

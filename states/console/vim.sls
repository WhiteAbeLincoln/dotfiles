install vim:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:vim:console', ['vim']) }}

include:
  - ..dotfiles.vim

install tmux:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:tmux', ['tmux']) }}

include:
  - ..dotfiles.tmux

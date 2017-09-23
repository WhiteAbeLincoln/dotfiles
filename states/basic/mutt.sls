install mutt:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:mutt', ['mutt']) }}

include:
  - ..dotfiles.mutt

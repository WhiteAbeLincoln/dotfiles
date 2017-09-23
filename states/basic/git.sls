install git:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:git', ['git']) }}

include:
  - ..dotfiles.git

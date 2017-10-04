install zsh:
  pkg.installed:
      - pkgs: {{ salt['pillar.get']('packages:zsh', ['zsh']) }}

install antibody:
  cmd.run:
    - name: >-
        curl -sL https://git.io/antibody | bash -s
    - unless: command -v antibody

include:
  - ..dotfiles.zsh

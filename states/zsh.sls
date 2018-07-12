# install zsh:
#   pkg.installed:
#       - pkgs: {{ salt['pillar.get']('packages:zsh', ['zsh']) }}

install grml config:
  file.managed:
    - name: /home/abe/.zshrc
    - source: https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
    - skip_verify: true
    - user: abe
    - group: abe

install antibody:
  cmd.run:
    - name: >-
        curl -sL https://git.io/antibody | bash -s
    - unless: command -v antibody

manage abes plugin bundles:
  file.managed:
    - name: /home/abe/.zbundles.txt
    - source: salt://dotfiles/zsh/zbundles.txt
    - user: abe
    - group: abe

manage abes zprofile:
  file.managed:
    - name: /home/abe/.zprofile
    - source: salt://dotfiles/zsh/zprofile.zsh
    - user: abe
    - group: abe

manage abes zaliases:
  file.managed:
    - name: /home/abe/.zaliases
    - source: salt://dotfiles/zsh/zaliases.zsh
    - user: abe
    - group: abe

manage abes zshenv:
  file.managed:
    - name: /home/abe/.zshenv
    - source: salt://dotfiles/zsh/zshenv.zsh
    - user: abe
    - group: abe

manage abes zshrc:
  file.managed:
    - name: /home/abe/.zshrc.local
    - source: salt://dotfiles/zsh/zshrc.zsh
    - user: abe
    - group: abe

update abes antibody bundles:
  cmd.run:
    - name: antibody bundle < /home/abe/.zbundles.txt > /home/abe/.zplugins
    - cwd: /home/abe
    - runas: abe

# vim:ft=yaml

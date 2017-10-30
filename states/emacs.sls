# vim:ft=yaml

emacs installed:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:emacs', ['emacs']) }}

spacemacs git installed:
  git.latest:
    - name: https://github.com/syl20bnr/spacemacs.git
    - target: /home/abe/.emacs.d
    - user: abe
     # - unless: ls /home/abe/.emacs.d

manage spacemacs file:
  file.managed:
    - name: /home/abe/.spacemacs
    - source: salt://dotfiles/spacemacs.el
    - user: abe
    - group: abe

# vim:ft=yaml

install haskell stack:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:stack', ['stack']) }}

ensure abes-xmonad repository:
  git.latest:
    - name: https://github.com/WhiteAbeLincoln/abes-xmonad.git
    - target: /home/abe/.xmonad
    - user: abe

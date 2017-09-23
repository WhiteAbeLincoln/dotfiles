uninstall graphical emacs:
  pkg.removed:
    - pkgs: {{ salt['pillar.get']('packages:emacs:graphical', ['emacs']) }}

install emacs:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:emacs:console', ['emacs-nox']) }}

include:
  - ..dotfiles.emacs

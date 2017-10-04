graphical emacs:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:emacs:graphical', ['emacs']) }}

uninstall emacs:
  pkg.removed:
    - pkgs: {{ salt['pillar.get']('packages:emacs:console', ['emacs-nox']) }}

include:
  - ..dotfiles.emacs

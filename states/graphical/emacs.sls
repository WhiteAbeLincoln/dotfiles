uninstall terminal emacs:
  pkg.removed:
      - pkgs: {{ salt['pillar.get']('packages:emacs:console', ['emacs-nox']) }}

install graphical emacs:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:emacs:graphical', ['emacs']) }}

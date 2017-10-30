# vim:ft=yaml
install sound stuff:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:pulseaudio', []) }}

install fonts:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:fonts', []) }}

install gtk themes:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:gtk_theme', []) }}

install a browser:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:browser', []) }}

install some compression utilites:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:compression', []) }}

install file manager:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:filemanager', []) }}

install an office suite:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:libreoffice', []) }}

install a pdf reader:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:zathura', []) }}

install video stuff:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:video', []) }}

install image manipulation stuff:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:images', []) }}

install latex:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:latex', []) }}

install sagemath:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:sagemath', []) }}

install utilites:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:utilites', []) }}

install calibre:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:calibre', []) }}

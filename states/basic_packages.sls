install clang:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:clang', ['clang']) }}

install stack:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:stack', ['stack']) }}

 

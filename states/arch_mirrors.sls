{% if grains['os'] == 'Arch' %}
reflector:
  pkg.installed

ensure hooks directory:
  file.directory:
    - name: /etc/pacman.d/hooks

manage pacman hook:
  file.managed:
    - name: /etc/pacman.d/hooks/mirrorupgrade.hook
    - source: salt://dotfiles/mirrorupgrade.hook
{% endif %}

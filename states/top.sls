base:
  '*':
    - locale
    - timezone
    - users
    - zsh
    - tmux.config
    - vim.config
    - git.config
  'localhost:raptor':
    - match: grain
    - xorg
    - termite
    - rofi
    - xmonad
    - desktop_packages
    - mutt
  'localhost:valkyrie':
    - match: grain
    - openssh
    - openssh.config
  # 'localhost:blackbird':
     # - match: grain

# vim:ft=yaml

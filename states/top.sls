base:
  '*':
    - locale
    - timezone
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
  # 'localhost:valkyrie':
  #   - match: grain
  #   - dns
  # 'localhost:blackbird':
     # - match: grain

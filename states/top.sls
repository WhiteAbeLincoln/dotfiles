base:
  '*':
    - users
    - locale
    - timezone
    - zsh
    - tmux.config
    - vim.config
    - git.config
    - mutt
    - network
  'localhost:raptor':
    - match: grain
    - xorg
    - termite
    - rofi
    - xmonad
    - desktop_packages
  # 'localhost:blackbird':
     # - match: grain

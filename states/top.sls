base:
  '*':
    - users
    - locale
    - timezone
    - zsh
    - tmux
    - vim
    - git
    - mutt
  'localhost:raptor':
     - match: grain
     - xorg
     - termite
  # 'localhost:blackbird':
     # - match: grain

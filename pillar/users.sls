users:
  abe:
    fullname: Abraham White
    sudouser: True
    ssh_keys_pillar:
      id_rsa: "secrets:vault:ssh_keys:abe"
    password: $1$0xhbScOH$TjidxpvEuF/H8b2oxJVSx.
    shell: /bin/zsh
    groups:
      - users
    optional_groups:
      - games
      - http
      - log
      - rfkill
      - sys
      - systemd-journal

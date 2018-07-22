users:
  abe:
    fullname: Abraham White
    sudouser: True
    ssh_keys_pillar:
      id_rsa: "secrets:vault:ssh_keys:abe"
    password: $1$0xhbScOH$TjidxpvEuF/H8b2oxJVSx.
    shell: /usr/bin/zsh
    groups:
      - users
    optional_groups:
      - games
      - cdrom
      - floppy
      - audio
      - dip
      - video
      - plugdev
      - netdev
      - lpadmin
      - log
      - sys
      - systemd-journal
      - scanner

# vim: ft=yaml

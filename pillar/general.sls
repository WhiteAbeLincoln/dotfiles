# vim:ft=yaml
sshd_config:
  # This keyword is totally optional
  AuthorizedKeysFile: '.ssh/authorized_keys'
  ChallengeResponseAuthentication: 'yes'
  AuthenticationMethods: 'publickey,keyboard-interactive:pam'
  UsePAM: 'yes'
  PrintMotd: 'no'

timezone:
  name: 'America/Denver'
  utc: True

locale:
  present:
    - "en_US.UTF-8 UTF-8"
  default:
    name: "en_US.UTF-8"
    requires: "en_US.UTF-8 UTF-8"

users:
  abe:
    fullname: Abraham White
    sudouser: True
    ssh_keys_pillar:
      id_rsa: "secrets:vault:ssh_keys:abe"
    password: $1$0xhbScOH$TjidxpvEuF/H8b2oxJVSx.

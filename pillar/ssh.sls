# vim:ft=yaml
sshd_config:
  # This keyword is totally optional
  AuthorizedKeysFile: '.ssh/authorized_keys'
  ChallengeResponseAuthentication: 'yes'
  AuthenticationMethods: 'publickey,keyboard-interactive:pam'
  UsePAM: 'yes'
  PrintMotd: 'no'

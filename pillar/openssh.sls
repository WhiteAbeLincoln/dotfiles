sshd_config:
  Port: 22
  Protocol: 2
  AllowUsers:
    - abe
  PermitRootLogin: no
  PasswordAuthentication: no
  UsePAM: yes
  ChallengeResponseAuthentication: yes
  AcceptEnv: 'LANG LC_*'
  Subsystem: 'sftp /usr/lib/openssh/sftp-server'
  AuthenticationMethods: 'publickey keyboard-interactive:pam'


openssh:
  sshd_enable: true

# vim: ft=yaml

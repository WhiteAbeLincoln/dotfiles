ddclient:
  hosts:
    "valkyrie.abrahamwhite.com":
        ssl: 'yes'
        protocol: googledomains
        login: {{ salt['pillar.get']('secrets:vault:ddclient:valkyrie:username') }}
        password: {{ salt['pillar.get']('secrets:vault:ddclient:valkyrie:password') }}

# vim: ft=yaml

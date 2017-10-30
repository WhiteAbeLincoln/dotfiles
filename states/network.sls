#vim: ft=yaml

include:
  - avahi

install network stuff:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:network', []) }}

manage nsswitch.conf:
  file.managed:
    - name: /etc/nsswitch.conf
    - source: salt://dotfiles/nsswitch.conf

install firewall:
  pkg.installed:
    - pkgs: {{ salt['pillar.get']('packages:firewall', ['iptables', 'firewalld']) }}

firewalld running:
  service.enabled:
    - name: firewalld

# firewall using firewalld

## Example iptables statefull firewall
# ensure TCP chain:
#   iptables.chain_present:
#     - name: TCP

# ensure UDP chain:
#   iptables.chain_present:
#     - name: UDP

# drop forward packets:
#   iptables.set_policy:
#     - chain: FORWARD
#     - policy: DROP

# accept output packets:
#   iptables.set_policy:
#     - chain: OUTPUT
#     - policy: ACCEPT

# drop input packets:
#   iptables.set_policy:
#     - chain: INPUT
#     - policy: DROP

# accept icmp:
#   iptables.append:
#     - chain: INPUT
#     - match: conntrack
#     - ctstate: RELATED,ESTABLISHED
#     - jump: ACCEPT

# accept loopback:
#   iptables.append:
#     - chain: INPUT
#     - in-interface: lo
#     - jump: ACCEPT

# accept ICMPv6 Neighbor Discovery:
#   iptables.append:
#     - chain: INPUT
#     - protocol: 41
#     - jump: ACCEPT

# drop invalid:
#   iptables.append:
#     - chain: INPUT
#     - match: conntrack
#     - ctstate: INVALID
#     - jump: DROP

# allow ICMP echo:
#   iptables.append:
#     - chain: INPUT
#     - match: conntrack
#     - ctstate: NEW
#     - protocol: icmp 
#     - icmp-type: 8
#     - jump: ACCEPT

# attach udp chain to input:
#   iptables.append:
#     - chain: INPUT
#     - protocol: udp
#     - match: conntrack
#     - ctstate: NEW
#     - jump: UDP

# attach tcp chain to input:
#   iptables.append:
#     - chain: INPUT
#     - protocol: tcp
#     - syn:
#     - match: conntrack
#     - ctstate: NEW
#     - jump: TCP

# setup udp rejections:
#   iptables.append:
#     - chain: INPUT
#     - protocol: udp
#     - jump: REJECT
#     - reject-with: icmp-port-unreachable

# setup tcp rejections:
#   iptables.append:
#     - chain: INPUT
#     - protocol: tcp
#     - jump: REJECT
#     - reject-with: tcp-reset

# setup other rejections:
#   iptables.append:
#     - chain: INPUT
#     - jump: REJECT
#     - reject-with: icmp-proto-unreachable

# allow ssh:
#   iptables.append:
#     - chain: TCP
#     - protocol: tcp
#     - dport: 22
#     - jump: ACCEPT


# LAN DNS resolver: answers the homelab wildcard privately (never in public DNS)
# and does network-wide ad-blocking. Replaces the mDNS alias scheme. Host-level
# (NOT a k3s workload) so it keeps resolving even if the cluster is unhealthy.
#
# The web UI (:3000) is bound on all interfaces but only reachable over
# tailscale0 (a trusted firewall interface) + localhost — port 53 is the only
# thing opened to the LAN, and only on the LAN interface (see default.nix).
{...}: let
  facts = import ./facts.nix;
  secrets = import ../../secrets/globalhawk.nix;
in {
  services.adguardhome = {
    enable = true;
    # Fully declarative: UI edits are reverted on restart, config lives in Nix.
    mutableSettings = false;
    # Web UI bind (module maps host/port -> http.address). Firewall gates LAN access.
    host = "0.0.0.0";
    port = 3000;
    openFirewall = false; # port 53 is scoped by interface in default.nix
    settings = {
      users = [
        {
          name = "admin";
          password = secrets.adguard_password_hash;
        }
      ];
      dns = {
        bind_hosts = ["0.0.0.0"];
        port = 53;
        # DoH upstreams; bootstrap_dns resolves the upstream hostnames and
        # satisfies the module's bootstrap assertion under mutableSettings=false.
        upstream_dns = [
          "https://dns.cloudflare.com/dns-query"
          "https://dns.quad9.net/dns-query"
        ];
        bootstrap_dns = ["1.1.1.1" "9.9.9.9"];
      };
      filtering = {
        protection_enabled = true;
        filtering_enabled = true;
        # REQUIRED since AdGuard schema 31 (v0.107.68): the global rewrites toggle
        # and the per-entry `enabled` both default to FALSE when omitted, so a
        # config without them loads cleanly but silently applies no rewrites
        # (queries fall through to upstream). See CHANGELOG v0.107.68.
        rewrites_enabled = true;
        # Split-horizon: the homelab wildcard resolves to globalhawk on the LAN.
        # These A records exist ONLY here — never in public DNS.
        rewrites = [
          {
            domain = "*${facts.ingressSuffix}";
            answer = facts.lanIp;
            enabled = true;
          }
        ];
      };
      # Ad-blocking blocklist(s).
      filters = [
        {
          enabled = true;
          name = "AdGuard DNS filter";
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
          id = 1;
        }
      ];
    };
  };
}

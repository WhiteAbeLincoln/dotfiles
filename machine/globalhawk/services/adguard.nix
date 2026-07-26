# LAN DNS resolver: answers the homelab wildcard privately (never in public DNS)
# and does network-wide ad-blocking. Replaces the mDNS alias scheme. Host-level
# (NOT a k3s workload) so it keeps resolving even if the cluster is unhealthy.
#
# The web UI (:3000) is bound on all interfaces but only reachable over
# tailscale0 (a trusted firewall interface) + localhost — port 53 is the only
# thing opened to the LAN, and only on the LAN interface (see default.nix).
{
  config,
  lib,
  ...
}: let
  lan = config.homelab.network;
  clusterNetwork = config.services.k3s.clusterNetwork;
  secrets = import ../../../secrets/globalhawk.nix;
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
            domain = "*${config.homelab.ingressSuffix}";
            answer = lan.lanIp;
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

  # AdGuard Home stays host-native (above) and is SSO-EXCEPTED (no proxy-header
  # trust -> forward-auth would only double-login one admin page). We only give
  # it a hostname + TLS + a routing-table row, exactly like Plex: Traefik ->
  # selector-less Service -> manual EndpointSlice -> the host's AdGuard web UI
  # (reachable from pods over the trusted cni0 bridge; no firewall change).
  # AdGuard's own admin login remains the gate.
  services.k3s.workloads.module = {nixosConfig, ...}: let
    host = "adguard${config.homelab.ingressSuffix}";
    port = nixosConfig.services.adguardhome.port;
  in {
    applications.adguard = {
      namespace = "adguard";
      createNamespace = true;
      resources = {
        services.adguard.spec.ports.web = {
          inherit port;
          targetPort = port;
        };
        ingresses.adguard.spec = {
          ingressClassName = "traefik";
          tls = [{hosts = [host];}];
          rules = [
            {
              inherit host;
              http.paths = [
                {
                  path = "/";
                  pathType = "Prefix";
                  backend.service = {
                    name = "adguard";
                    port.number = port;
                  };
                }
              ];
            }
          ];
        };
      };
      yamls = [
        (builtins.toJSON {
          apiVersion = "discovery.k8s.io/v1";
          kind = "EndpointSlice";
          metadata = {
            name = "adguard";
            namespace = "adguard";
            labels."kubernetes.io/service-name" = "adguard";
          };
          addressType = "IPv4";
          endpoints = [{addresses = [clusterNetwork.hostGatewayIp];}];
          ports = [
            {
              name = "web";
              inherit port;
              protocol = "TCP";
            }
          ];
        })
      ];
    };
  };
}

# torrent-vpn: gluetun (Mullvad WireGuard) + qbittorrent in ONE pod, sharing the
# pod network namespace — the k8s-native replacement for docker's
# --network=container:vpn. Only qbittorrent egresses via the VPN; the arr apps
# do not (they reach qbit by cluster DNS). The WG key is a k8s Secret
# (mullvad-wg / WIREGUARD_PRIVATE_KEY) rendered by sops-nix into the k3s
# manifests dir at activation — see machine/globalhawk/sops.nix — never in the
# store or git. gluetun references it by name via secretKeyRef below.
{
  lib,
  ingressSuffix,
  wireguardAddresses,
  vpnServerCities,
  podCidr,
  serviceCidr,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  labels = l.appLabels "qbittorrent";
  host = "qbittorrent${ingressSuffix}";
in {
  applications.torrent = {
    namespace = "media";
    createNamespace = false;
    resources = {
      deployments.torrent-vpn.spec = {
        replicas = 1;
        selector.matchLabels = labels;
        # qbit holds a SQLite/config lock; never overlap two instances.
        strategy.type = "Recreate";
        template = {
          metadata.labels = labels;
          spec = {
            securityContext.fsGroup = mediaUid;
            volumes = [
              {
                name = "tun";
                hostPath = {
                  path = "/dev/net/tun";
                  type = "CharDevice";
                };
              }
              {
                name = "qbt-config";
                hostPath = {
                  path = "${mediaRoot}/docker-services/torrent-config/qbittorrent";
                  type = "Directory";
                };
              }
              {
                name = "downloads";
                hostPath = {
                  path = "${mediaRoot}/torrents/downloads";
                  type = "Directory";
                };
              }
            ];
            containers = {
              # Network provider + killswitch. NET_ADMIN + /dev/net/tun for the
              # WireGuard interface. FIREWALL_OUTBOUND_SUBNETS re-opens the cluster
              # pod+service CIDRs through the killswitch so the WebUI is reachable
              # and qbit can resolve cluster DNS; FIREWALL_INPUT_PORTS allows
              # inbound to the WebUI port.
              gluetun = {
                image = "qmcgaw/gluetun";
                securityContext.capabilities.add = ["NET_ADMIN"];
                # gluetun self-heals its tunnel; this restarts the container as a
                # backstop if the VPN stays down (the "silent drop" case) so the
                # netns's WireGuard interface is rebuilt.
                livenessProbe = {
                  exec.command = ["/gluetun-entrypoint" "healthcheck"];
                  initialDelaySeconds = 30;
                  periodSeconds = 30;
                  timeoutSeconds = 10;
                  failureThreshold = 4;
                };
                volumeMounts = [
                  {
                    name = "tun";
                    mountPath = "/dev/net/tun";
                  }
                ];
                env = [
                  {
                    name = "TZ";
                    value = timezone;
                  }
                  {
                    name = "VPN_TYPE";
                    value = "wireguard";
                  }
                  {
                    name = "VPN_SERVICE_PROVIDER";
                    value = "mullvad";
                  }
                  {
                    name = "WIREGUARD_ADDRESSES";
                    value = wireguardAddresses;
                  }
                  {
                    name = "SERVER_CITIES";
                    value = vpnServerCities;
                  }
                  {
                    name = "FIREWALL_OUTBOUND_SUBNETS";
                    value = "${podCidr},${serviceCidr}";
                  }
                  {
                    name = "FIREWALL_INPUT_PORTS";
                    value = "9091";
                  }
                  {
                    name = "WIREGUARD_PRIVATE_KEY";
                    valueFrom.secretKeyRef = {
                      name = "mullvad-wg";
                      key = "WIREGUARD_PRIVATE_KEY";
                    };
                  }
                ];
              };
              # Shares gluetun's netns (same pod) -> all its traffic transits the
              # VPN, exactly as --network=container:vpn did. VueTorrent installed
              # by the LinuxServer mod at /vuetorrent (matches the existing
              # WebUI\RootFolder=/vuetorrent config).
              qbittorrent = {
                image = "lscr.io/linuxserver/qbittorrent:latest";
                # No runAsUser: LinuxServer starts as root and drops to PUID/PGID
                # via s6 (required for the VueTorrent docker mod to install).
                ports.webui.containerPort = 9091;
                # Ready only when the WebUI answers, so the ingress doesn't 502
                # during restarts / the VueTorrent mod install.
                readinessProbe = {
                  httpGet = {
                    path = "/";
                    port = 9091;
                  };
                  initialDelaySeconds = 20;
                  periodSeconds = 15;
                  timeoutSeconds = 8;
                  failureThreshold = 4;
                };
                # Restart qbit if it can't reach the internet through the VPN for a
                # sustained window (after gluetun has had time to recover) — the
                # automated version of the old manual "restart vpn, then qbit".
                livenessProbe = {
                  exec.command = [
                    "sh"
                    "-c"
                    "wget -q -T 8 -O /dev/null http://connectivitycheck.gstatic.com/generate_204 || curl -fsS -m 8 -o /dev/null http://connectivitycheck.gstatic.com/generate_204"
                  ];
                  initialDelaySeconds = 90;
                  periodSeconds = 30;
                  timeoutSeconds = 12;
                  failureThreshold = 6;
                };
                env = [
                  {
                    name = "TZ";
                    value = timezone;
                  }
                  {
                    name = "PUID";
                    value = toString mediaUid;
                  }
                  {
                    name = "PGID";
                    value = toString mediaUid;
                  }
                  {
                    name = "WEBUI_PORT";
                    value = "9091";
                  }
                  {
                    name = "TORRENTING_PORT";
                    value = "6881";
                  }
                  {
                    name = "DOCKER_MODS";
                    value = "ghcr.io/gabe565/linuxserver-mod-vuetorrent";
                  }
                ];
                volumeMounts = [
                  {
                    name = "qbt-config";
                    mountPath = "/config";
                  }
                  {
                    name = "downloads";
                    mountPath = "/data/torrents/downloads";
                  }
                ];
              };
            };
          };
        };
      };
      services.qbittorrent.spec = {
        selector = labels;
        ports.webui = {
          port = 9091;
          targetPort = 9091;
        };
      };
      ingresses.qbittorrent = {
        spec = {
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
                    name = "qbittorrent";
                    port.number = 9091;
                  };
                }
              ];
            }
          ];
        };
      };
    };
  };
}

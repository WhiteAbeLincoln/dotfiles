# torrent-vpn: gluetun (Mullvad WireGuard) + qbittorrent in ONE pod, sharing the
# pod network namespace — the k8s-native replacement for docker's
# --network=container:vpn. Only qbittorrent egresses via the VPN; the arr apps
# do not (they reach qbit by cluster DNS). The WG key is a k8s Secret
# (mullvad-wg / WIREGUARD_PRIVATE_KEY) rendered by sops-nix into the k3s
# manifests dir at activation — see machine/globalhawk/sops.nix — never in the
# store or git. gluetun references it by name via secretKeyRef below.
{
  lib,
  config,
  ...
}: let
  secrets = import ../../../secrets/globalhawk.nix;
  ingressSuffix = config.homelab.ingressSuffix;
  wireguardAddresses = secrets.wireguard_addresses;
  vpnServerCities = secrets.vpn_server_cities;
  podCidr = config.services.k3s.clusterNetwork.podCidr;
  serviceCidr = config.services.k3s.clusterNetwork.serviceCidr;
  mediaRoot = config.homelab.media.root;
  mediaUid = config.users.users._media.uid;
  timezone = config.time.timeZone;
  gluetunImage = "qmcgaw/gluetun@sha256:ad6b604e0cecc917a5cb6a8de55cd167ba415da8b7ec13456abb871a84be3c30";
in {
  services.k3s.workloads.module = {k8sLib, ...}: let
    l = k8sLib;
    labels = l.appLabels "qbittorrent";
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
              # Keep a pristine resolver file for every Gluetun process start.
              # Kubernetes retains the pod's resolver file across container
              # restarts, after Gluetun has changed its nameserver to localhost.
              initContainers.save-cluster-dns = {
                image = gluetunImage;
                command = ["/bin/sh" "-ec" "cp /etc/resolv.conf /vpn-startup/resolv.conf"];
                volumeMounts = [
                  {
                    name = "vpn-startup";
                    mountPath = "/vpn-startup";
                  }
                ];
              };
              volumes = [
                {
                  name = "vpn-startup";
                  emptyDir = {};
                }
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
                  image = gluetunImage;
                  securityContext.capabilities.add = ["NET_ADMIN"];
                  # Container restarts retain the pod network namespace. Remove
                  # WireGuard policy rules left by an interrupted prior process
                  # before gluetun configures table 51820 again. Restore cluster
                  # DNS before discovery so local queries never forward to the
                  # resolver itself. An entrypoint wrapper orders both operations
                  # before Gluetun; postStart would race its initialization.
                  command = [
                    "/bin/sh"
                    "-ec"
                    ''
                      cat /vpn-startup/resolv.conf > /etc/resolv.conf
                      while ip rule del table 51820 2>/dev/null; do :; done
                      while ip -6 rule del table 51820 2>/dev/null; do :; done
                      exec /gluetun-entrypoint
                    ''
                  ];
                  # Gluetun self-heals its tunnel. A prolonged VPN outage makes
                  # the pod unready without repeatedly restarting its containers.
                  readinessProbe = {
                    exec.command = ["/gluetun-entrypoint" "healthcheck"];
                    initialDelaySeconds = 30;
                    periodSeconds = 30;
                    timeoutSeconds = 10;
                    failureThreshold = 4;
                  };
                  resources = {
                    requests = {
                      cpu = "100m";
                      memory = "128Mi";
                    };
                    limits = {
                      cpu = "500m";
                      memory = "512Mi";
                    };
                  };
                  volumeMounts = [
                    {
                      name = "vpn-startup";
                      mountPath = "/vpn-startup";
                      readOnly = true;
                    }
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
                # No runAsUser: LinuxServer starts as root and drops to PUID/PGID
                # via s6 (required for the VueTorrent docker mod to install).
                qbittorrent =
                  (l.mkLsioContainer {
                    name = "qbittorrent";
                    image = "lscr.io/linuxserver/qbittorrent@sha256:b024436f8ca665d16d9a997d26fd27fdf867ee5566ba09f32764e7b2976d3e02";
                    port = 9091;
                    portName = "webui";
                    inherit mediaUid timezone;
                    configVolumeName = "qbt-config";
                    extraEnv = [
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
                    extraMounts = [
                      {
                        name = "downloads";
                        mountPath = "/data/torrents/downloads";
                      }
                    ];
                    probes = {
                      # Ready only when the WebUI answers, keeping ordinary clients
                      # away during restarts / the VueTorrent mod install. The diagnostic
                      # WebUI service publishes unready endpoints, so it can still
                      # return transient errors while the application starts.
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
                      # Restart qbit only when its own WebUI stops responding;
                      # restarting it cannot repair an unavailable VPN subscription.
                      startupProbe = {
                        httpGet = {
                          path = "/";
                          port = 9091;
                        };
                        periodSeconds = 10;
                        timeoutSeconds = 8;
                        failureThreshold = 60;
                      };
                      livenessProbe = {
                        httpGet = {
                          path = "/";
                          port = 9091;
                        };
                        periodSeconds = 30;
                        timeoutSeconds = 12;
                        failureThreshold = 6;
                      };
                    };
                  })
                  // {
                    resources = {
                      requests = {
                        cpu = "100m";
                        memory = "256Mi";
                      };
                      limits = {
                        cpu = "1";
                        memory = "1Gi";
                      };
                    };
                  };
              };
            };
          };
        };
        services =
          (l.mkService {
            name = "qbittorrent";
            port = 9091;
            portName = "webui";
          })
          // {
            # Keep the authenticated WebUI reachable for diagnosis while VPN
            # readiness removes the pod from the normal qbittorrent service.
            qbittorrent-webui.spec = {
              selector = labels;
              publishNotReadyAddresses = true;
              ports.webui = {
                port = 9091;
                targetPort = 9091;
              };
            };
          };
        ingresses = l.mkIngress {
          name = "qbittorrent-webui";
          port = 9091;
          host = "qbittorrent${ingressSuffix}";
          forwardAuth = true;
        };
      };
    };
  };
}

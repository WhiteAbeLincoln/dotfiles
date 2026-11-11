# Audiobookshelf: the audiobook manager. Embedded SQLite (no DB server), no
# OPDS. Uses the native service account and /var/lib/audiobookshelf state.
# Audiobooks remain on the media dataset; the UI scans that directory directly.
{config, ...}: let
  ingressSuffix = config.homelab.ingressSuffix;
  mediaRoot = config.homelab.media.root;
  timezone = config.time.timeZone;
  cfg = config.services.audiobookshelf;
  clusterNetwork = config.services.k3s.clusterNetwork;
in {
  # The host deliberately uses public DNS, while this OIDC issuer exists only
  # in homelab DNS. Native discovery and token exchange must reach Traefik too.
  networking.hosts.${config.homelab.network.lanIp} = ["auth${ingressSuffix}"];

  services.audiobookshelf = {
    enable = true;
    host = "0.0.0.0";
    port = 13378;
    # Pods reach the host over trusted cni0; no LAN firewall opening is needed.
    openFirewall = false;
  };
  systemd.services.audiobookshelf = {
    # SQLite must have only one writer. Stop the service before copying state.
    environment.TZ = timezone;
    unitConfig.RequiresMountsFor = ["${mediaRoot}/audiobooks"];
    serviceConfig = {
      # Keep startup and state management upstream. Limit additional writes to
      # the audiobook library; StateDirectory is writable automatically.
      SupplementaryGroups = ["_media"];
      ReadWritePaths = ["${mediaRoot}/audiobooks"];
      StateDirectoryMode = "0700";
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      NoNewPrivileges = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
      UMask = "0002";
    };
  };
  # Keep Traefik TLS and the existing cluster DNS name, as for native Plex.
  services.k3s.workloads.module = {k8sLib, ...}: let
    l = k8sLib;
  in {
    applications.audiobookshelf = {
      namespace = "library";
      createNamespace = false;
      resources = {
        services.audiobookshelf.spec.ports.http = {
          port = cfg.port;
          targetPort = cfg.port;
        };
        ingresses = l.mkIngress {
          name = "audiobookshelf";
          port = cfg.port;
          host = "audiobooks${ingressSuffix}";
        };
      };
      yamls = [
        (builtins.toJSON {
          apiVersion = "discovery.k8s.io/v1";
          kind = "EndpointSlice";
          metadata = {
            name = "audiobookshelf";
            namespace = "library";
            labels."kubernetes.io/service-name" = "audiobookshelf";
          };
          addressType = "IPv4";
          endpoints = [{addresses = [clusterNetwork.hostGatewayIp];}];
          ports = [
            {
              name = "http";
              port = cfg.port;
              protocol = "TCP";
            }
          ];
        })
      ];
    };
  };
}

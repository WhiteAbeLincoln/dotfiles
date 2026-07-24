# Shared helpers for the globalhawk nixidy modules.
{lib}: rec {
  # Common labels applied to every workload we author, so NetworkPolicy and
  # kubectl selectors have a stable handle.
  appLabels = name: {
    "app.kubernetes.io/name" = name;
    "app.kubernetes.io/managed-by" = "nixidy";
  };
  # A ClusterIP Service selecting this app's pods on a single named port.
  # portName defaults to "http"; the torrent pod uses "webui" to match its
  # existing manifest.
  mkService = {
    name,
    port,
    portName ? "http",
  }: {
    "${name}".spec = {
      selector = appLabels name;
      ports.${portName} = {
        inherit port;
        targetPort = port;
      };
    };
  };

  # A Traefik Ingress routing `host` to this app's Service on `port`. No
  # secretName: Traefik serves its default *.h wildcard cert. `host` is passed
  # explicitly so an app's ingress hostname can differ from its resource name
  # (e.g. calibre-web-automated -> books.h.…).
  mkIngress = {
    name,
    port,
    host,
  }: {
    "${name}".spec = {
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
                inherit name;
                port.number = port;
              };
            }
          ];
        }
      ];
    };
  };
  # mkArrApp builds the Deployment+Service+Ingress triple shared by every
  # linuxserver.io *arr app. Config dir is hostPath-mounted in place (no data
  # copy); PUID/PGID semantics preserved via runAsUser/Group/fsGroup = 994 (the
  # _media uid/gid). Returns an `applications.<name>` fragment for mkMerge.
  mkArrApp = {
    name,
    image,
    port,
    ingressSuffix,
    mediaRoot,
    mediaUid,
    timezone,
    extraVolumes ? [],
    extraMounts ? [],
  }: let
    labels = appLabels name;
  in {
    "${name}" = {
      namespace = "media";
      createNamespace = false;
      resources = {
        deployments."${name}".spec = {
          replicas = 1;
          selector.matchLabels = labels;
          # arr apps hold a SQLite lock on /config; never run two at once.
          strategy.type = "Recreate";
          template = {
            metadata.labels = labels;
            spec = {
              # LinuxServer images START as root and drop to PUID/PGID via s6;
              # forcing runAsUser breaks their init (mods, permission fixups). So
              # run as root + PUID/PGID env, with fsGroup for volume ownership.
              securityContext.fsGroup = mediaUid;
              containers."${name}" = {
                inherit image;
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
                ];
                ports.http.containerPort = port;
                volumeMounts =
                  [
                    {
                      name = "config";
                      mountPath = "/config";
                    }
                  ]
                  ++ extraMounts;
              };
              volumes =
                [
                  {
                    name = "config";
                    hostPath = {
                      path = "${mediaRoot}/docker-services/torrent-config/${name}";
                      type = "Directory";
                    };
                  }
                ]
                ++ extraVolumes;
            };
          };
        };
        services = mkService {inherit name port;};
        ingresses = mkIngress {
          inherit name port;
          host = "${name}${ingressSuffix}";
        };
      };
    };
  };
}

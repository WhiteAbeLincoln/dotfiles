# Shared helpers for the globalhawk nixidy modules.
{lib}: rec {
  # Common labels applied to every workload we author, so NetworkPolicy and
  # kubectl selectors have a stable handle.
  appLabels = name: {
    "app.kubernetes.io/name" = name;
    "app.kubernetes.io/managed-by" = "nixidy";
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
    host = "${name}${ingressSuffix}";
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
        services."${name}".spec = {
          selector = labels;
          ports.http = {
            port = port;
            targetPort = port;
          };
        };
        ingresses."${name}" = {
          metadata.annotations."cert-manager.io/cluster-issuer" = "globalhawk-ca";
          spec = {
            ingressClassName = "traefik";
            tls = [
              {
                hosts = [host];
                secretName = "${name}-tls";
              }
            ];
            rules = [
              {
                inherit host;
                http.paths = [
                  {
                    path = "/";
                    pathType = "Prefix";
                    backend.service = {
                      name = name;
                      port.number = port;
                    };
                  }
                ];
              }
            ];
          };
        };
      };
    };
  };
}

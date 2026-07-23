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
    extraVolumes ? [],
    extraMounts ? [],
  }: let
    labels = appLabels name;
    host = "${name}${ingressSuffix}";
    mediaUid = 994;
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
              securityContext = {
                runAsUser = mediaUid;
                runAsGroup = mediaUid;
                fsGroup = mediaUid;
              };
              containers."${name}" = {
                inherit image;
                env = [
                  {
                    name = "TZ";
                    value = "America/Denver";
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
                      path = "/data/Media/docker-services/torrent-config/${name}";
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

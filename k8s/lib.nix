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
  # secretName: Traefik serves its default cert (the *.h wildcard,
  # kube-system TLSStore/default), so no per-app issuer or cert is needed.
  # `host` is passed explicitly so an app's ingress hostname can differ from
  # its resource name (e.g. calibre-web-automated -> books.h.…).
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
  # The env every LinuxServer.io image shares: timezone + PUID/PGID set to the
  # _media uid/gid (994) so files land _media-owned. LSIO images start as root
  # and drop to PUID/PGID via s6, so no runAsUser.
  lsioEnv = {
    mediaUid,
    timezone,
  }: [
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

  # A single LinuxServer.io container: shared env (+ extraEnv), one named port,
  # a /config mount (+ extraMounts), optional probes. Used standalone inside the
  # torrent pod and as the container of mkLsioApp.
  mkLsioContainer = {
    name,
    image,
    port,
    mediaUid,
    timezone,
    portName ? "http",
    configVolumeName ? "config",
    configMountPath ? "/config",
    extraEnv ? [],
    extraMounts ? [],
    probes ? {},
  }:
    {
      inherit image;
      env = lsioEnv {inherit mediaUid timezone;} ++ extraEnv;
      ports.${portName}.containerPort = port;
      volumeMounts =
        [
          {
            name = configVolumeName;
            mountPath = configMountPath;
          }
        ]
        ++ extraMounts;
    }
    // probes;

  # A standalone single-container LSIO app: Deployment (fsGroup=994, Recreate,
  # /config hostPath from configPath) + Service + Ingress. Replaces mkArrApp.
  # `host` defaults to name-based but can be overridden (books.h.… for CWA).
  mkLsioApp = {
    name,
    image,
    port,
    ingressSuffix,
    mediaUid,
    timezone,
    configPath,
    namespace ? "media",
    portName ? "http",
    host ? "${name}${ingressSuffix}",
    extraVolumes ? [],
    extraMounts ? [],
    extraEnv ? [],
    ...
  }: let
    labels = appLabels name;
  in {
    "${name}" = {
      inherit namespace;
      createNamespace = false;
      resources = {
        deployments."${name}".spec = {
          replicas = 1;
          selector.matchLabels = labels;
          # Holds a SQLite/config lock on /config; never run two at once.
          strategy.type = "Recreate";
          template = {
            metadata.labels = labels;
            spec = {
              # LinuxServer images START as root and drop to PUID/PGID via s6;
              # forcing runAsUser breaks their init (mods, permission fixups). So
              # run as root + PUID/PGID env, with fsGroup for volume ownership.
              securityContext.fsGroup = mediaUid;
              containers."${name}" = mkLsioContainer {
                inherit name image port portName mediaUid timezone extraEnv extraMounts;
              };
              volumes =
                [
                  {
                    name = "config";
                    hostPath = {
                      path = configPath;
                      type = "Directory";
                    };
                  }
                ]
                ++ extraVolumes;
            };
          };
        };
        services = mkService {inherit name port portName;};
        ingresses = mkIngress {inherit name port host;};
      };
    };
  };
}

# Immich on k3s (v3.0.0), fresh DB. Four workloads in the `immich` namespace:
# server + machine-learning (share ONE version tag — the upgrade knob), the
# official VectorChord Postgres, and Valkey. All run as the dedicated `immich`
# uid (988, NOT _media) so the data tree stays isolated; storage is hostPath
# under ${mediaRoot}/immich (owned/ACL'd by machine/globalhawk/immich-storage.nix).
# DB password from the sops `immich-db` Secret. Mirrors the official Helm chart's
# shape. See docs/superpowers/specs/2026-07-24-globalhawk-immich-k3s-design.md.
{
  lib,
  ingressSuffix,
  mediaRoot,
  timezone,
  immichUid,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  version = "v3.0.0";
  serverImage = "ghcr.io/immich-app/immich-server:${version}@sha256:685ba5c93337058ff8a189d3ed89f0ba470ef966b1c94d2eace1a3d991f9816e";
  mlImage = "ghcr.io/immich-app/immich-machine-learning:${version}@sha256:5b480e92a2b77618d9ccae8c8110b0eae144ec9daf86715d246ec6d39cb7a553";
  pgImage = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:bcf63357191b76a916ae5eb93464d65c07511da41e3bf7a8416db519b40b1c23";
  valkeyImage = "docker.io/valkey/valkey:9@sha256:8e8d64b405ce18f41b8e5ee20aa4687a8ed0022d1298f2ce31cdcf3a76e09411";

  serverLabels = l.appLabels "immich-server";
  mlLabels = l.appLabels "immich-machine-learning";
  pgLabels = l.appLabels "immich-postgres";
  redisLabels = l.appLabels "immich-redis";

  secCtx = {
    runAsUser = immichUid;
    runAsGroup = immichUid;
    fsGroup = immichUid;
  };
  dbPassword = {
    name = "DB_PASSWORD";
    valueFrom.secretKeyRef = {
      name = "immich-db";
      key = "password";
    };
  };
in {
  applications.immich = {
    namespace = "immich";
    createNamespace = false;
    resources = {
      deployments = {
        # --- Postgres (VectorChord) ---
        immich-postgres.spec = {
          replicas = 1;
          selector.matchLabels = pgLabels;
          strategy.type = "Recreate"; # single writer on the data dir
          template = {
            metadata.labels = pgLabels;
            spec = {
              securityContext = secCtx;
              containers.postgres = {
                image = pgImage;
                env = [
                  (dbPassword // {name = "POSTGRES_PASSWORD";})
                  {
                    name = "POSTGRES_USER";
                    value = "postgres";
                  }
                  {
                    name = "POSTGRES_DB";
                    value = "immich";
                  }
                  {
                    name = "POSTGRES_INITDB_ARGS";
                    value = "--data-checksums";
                  }
                ];
                ports.postgres.containerPort = 5432;
                volumeMounts = [
                  {
                    name = "pgdata";
                    mountPath = "/var/lib/postgresql/data";
                  }
                  {
                    name = "shm";
                    mountPath = "/dev/shm";
                  }
                ];
                readinessProbe.exec.command = ["pg_isready" "-U" "postgres" "-d" "immich"];
              };
              volumes = [
                {
                  name = "pgdata";
                  hostPath = {
                    path = "${mediaRoot}/immich/pgdata";
                    type = "Directory";
                  };
                }
                {
                  name = "shm";
                  emptyDir = {
                    medium = "Memory";
                    sizeLimit = "128Mi";
                  };
                }
              ];
            };
          };
        };

        # --- Valkey (redis) ---
        immich-redis.spec = {
          replicas = 1;
          selector.matchLabels = redisLabels;
          template = {
            metadata.labels = redisLabels;
            spec = {
              securityContext = secCtx;
              containers.redis = {
                image = valkeyImage;
                ports.redis.containerPort = 6379;
                readinessProbe.exec.command = ["redis-cli" "ping"];
              };
              # Job queue/cache only — safe to lose on restart.
            };
          };
        };

        # --- Machine learning (CPU) ---
        immich-machine-learning.spec = {
          replicas = 1;
          selector.matchLabels = mlLabels;
          strategy.type = "Recreate"; # holds the model-cache hostPath
          template = {
            metadata.labels = mlLabels;
            spec = {
              securityContext = secCtx;
              containers.machine-learning = {
                image = mlImage;
                env = [
                  {
                    name = "TRANSFORMERS_CACHE";
                    value = "/cache";
                  }
                  {
                    name = "HF_XET_CACHE";
                    value = "/cache/huggingface-xet";
                  }
                  {
                    name = "MPLCONFIGDIR";
                    value = "/cache/matplotlib-config";
                  }
                ];
                ports.http.containerPort = 3003;
                volumeMounts = [
                  {
                    name = "model-cache";
                    mountPath = "/cache";
                  }
                ];
                resources = {
                  requests.memory = "512Mi";
                  limits.memory = "3Gi";
                };
              };
              volumes = [
                {
                  name = "model-cache";
                  hostPath = {
                    path = "${mediaRoot}/immich/model-cache";
                    type = "Directory";
                  };
                }
              ];
            };
          };
        };

        # --- Server (API + web) ---
        immich-server.spec = {
          replicas = 1;
          selector.matchLabels = serverLabels;
          strategy.type = "Recreate"; # single writer on the library hostPath
          template = {
            metadata.labels = serverLabels;
            spec = {
              securityContext = secCtx;
              containers.server = {
                image = serverImage;
                env = [
                  dbPassword
                  {
                    name = "DB_HOSTNAME";
                    value = "immich-postgres";
                  }
                  {
                    name = "DB_USERNAME";
                    value = "postgres";
                  }
                  {
                    name = "DB_DATABASE_NAME";
                    value = "immich";
                  }
                  {
                    name = "REDIS_HOSTNAME";
                    value = "immich-redis";
                  }
                  {
                    name = "IMMICH_MACHINE_LEARNING_URL";
                    value = "http://immich-machine-learning:3003";
                  }
                  {
                    name = "TZ";
                    value = timezone;
                  }
                ];
                ports.http.containerPort = 2283;
                volumeMounts = [
                  {
                    name = "library";
                    mountPath = "/data";
                  }
                ];
                readinessProbe.httpGet = {
                  path = "/api/server/ping";
                  port = 2283;
                };
                livenessProbe.httpGet = {
                  path = "/api/server/ping";
                  port = 2283;
                };
              };
              volumes = [
                {
                  name = "library";
                  hostPath = {
                    path = "${mediaRoot}/immich/library";
                    type = "Directory";
                  };
                }
              ];
            };
          };
        };
      };

      services =
        (l.mkService {
          name = "immich-server";
          port = 2283;
        })
        // (l.mkService {
          name = "immich-postgres";
          port = 5432;
          portName = "postgres";
        })
        // (l.mkService {
          name = "immich-redis";
          port = 6379;
          portName = "redis";
        })
        // (l.mkService {
          name = "immich-machine-learning";
          port = 3003;
        });

      ingresses = l.mkIngress {
        name = "immich-server";
        port = 2283;
        host = "photos${ingressSuffix}";
      };
    };
  };
}

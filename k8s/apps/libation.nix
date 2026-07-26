{
  lib,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  labels = l.appLabels "libation-reconcile";
in {
  applications.libation = {
    namespace = "library";
    createNamespace = false;
    resources = {
      configMaps.libation-settings.data."Settings.json" = builtins.toJSON {
        ImportEpisodes = false;
        DownloadEpisodes = false;
        AutoDownloadEpisodes = false;
        # Libation 13.5.1 supports these property names and uses <id> for the
        # Audible product ID. Keep it in both paths so every output form is safe.
        FolderTemplate = "<title short> [<id>]";
        FileTemplate = "<title> [<id>]";
      };
      cronJobs.libation-reconcile.spec = {
        schedule = "0 4 1,15 * *";
        timeZone = timezone;
        concurrencyPolicy = "Forbid";
        successfulJobsHistoryLimit = 2;
        failedJobsHistoryLimit = 3;
        jobTemplate.spec = {
          backoffLimit = 2;
          activeDeadlineSeconds = 172800;
          template = {
            metadata.labels = labels;
            spec = {
              restartPolicy = "Never";
              securityContext = {
                runAsUser = mediaUid;
                runAsGroup = mediaUid;
                fsGroup = mediaUid;
              };
              initContainers.jitter = {
                image = "busybox:1.37.0@sha256:9532d8c39891ca2ecde4d30d7710e01fb739c87a8b9299685c63704296b16028";
                command = [
                  "/bin/sh"
                  "-eu"
                  "-c"
                  ''
                    # 4294964440 is the largest multiple of 7201 below 2^32.
                    # Reject the short tail so modulo maps equal-sized source sets.
                    while true; do
                      value="$(od -An -N4 -tu4 /dev/urandom | tr -d '[:space:]')"
                      case "$value" in
                        *[!0-9]*|"") exit 1 ;;
                      esac
                      test "$value" -lt 4294964440 && break
                    done
                    delay=$((value % 7201))
                    test "$delay" -ge 0
                    test "$delay" -le 7200
                    echo "scheduled jitter: ''${delay}s"
                    sleep "$delay"
                  ''
                ];
              };
              containers.libation = {
                image = "rmcrackan/libation:13.5.1@sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0";
                command = [
                  "/bin/sh"
                  "-eu"
                  "-c"
                  ''
                    # The 13.5.1 entrypoint copies AccountsSettings.json into
                    # /config-internal, where scan may refresh its tokens. Copy
                    # that mutable state back without hiding scan/liberate errors.
                    set +e
                    /libation/liberate.sh
                    status=$?
                    set -e
                    if test "$status" -eq 0; then
                      if ! cp /config-internal/AccountsSettings.json /config/AccountsSettings.json; then
                        echo "failed to persist refreshed Libation account state" >&2
                        exit 74
                      fi
                    fi
                    exit "$status"
                  ''
                ];
                env = [
                  {
                    name = "TZ";
                    value = timezone;
                  }
                  {
                    name = "SLEEP_TIME";
                    value = "-1";
                  }
                  {
                    name = "LIBATION_BOOKS_DIR";
                    value = "/data";
                  }
                  {
                    name = "LIBATION_CONFIG_DIR";
                    value = "/config";
                  }
                  {
                    name = "LIBATION_DB_DIR";
                    value = "/db";
                  }
                ];
                volumeMounts = [
                  {
                    name = "books";
                    mountPath = "/data";
                  }
                  {
                    name = "config";
                    mountPath = "/config";
                  }
                  {
                    name = "db";
                    mountPath = "/db";
                  }
                  # Libation 13.5.1's container forces Settings.InProgress to /tmp.
                  {
                    name = "in-progress";
                    mountPath = "/tmp";
                  }
                  {
                    name = "settings";
                    mountPath = "/config/Settings.json";
                    subPath = "Settings.json";
                  }
                ];
              };
              volumes = [
                {
                  name = "books";
                  hostPath = {
                    path = "${mediaRoot}/audiobooks";
                    type = "Directory";
                  };
                }
                {
                  name = "config";
                  hostPath = {
                    path = "${mediaRoot}/apps/libation/config";
                    type = "Directory";
                  };
                }
                {
                  name = "db";
                  hostPath = {
                    path = "${mediaRoot}/apps/libation/db";
                    type = "Directory";
                  };
                }
                {
                  name = "in-progress";
                  hostPath = {
                    path = "${mediaRoot}/apps/libation/in-progress";
                    type = "Directory";
                  };
                }
                {
                  name = "settings";
                  configMap.name = "libation-settings";
                }
              ];
            };
          };
        };
      };
    };
  };
}

# Audiobookshelf: the audiobook manager. Embedded SQLite (no DB server), no
# OPDS. Not a LinuxServer image (Node app: PORT env, arbitrary UID), so it is
# hand-rolled and runs as _media (994) so files it writes to the library stay
# _media-owned. Reads ${mediaRoot}/audiobooks (already backed up). Local auth
# now; native OIDC wired to Authelia later. See the design spec.
{
  lib,
  ingressSuffix,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  labels = l.appLabels "audiobookshelf";
in {
  applications.audiobookshelf = {
    namespace = "library";
    createNamespace = false;
    resources = {
      deployments.audiobookshelf.spec = {
        replicas = 1;
        selector.matchLabels = labels;
        # Holds a SQLite lock on /config; never run two at once.
        strategy.type = "Recreate";
        template = {
          metadata.labels = labels;
          spec = {
            securityContext = {
              runAsUser = mediaUid;
              runAsGroup = mediaUid;
              fsGroup = mediaUid;
            };
            containers.audiobookshelf = {
              image = "ghcr.io/advplyr/audiobookshelf:2.35.1@sha256:1eef6716183c52abafe5405e7d6be8390248ecd59c7488c44af871757ac8fc4d";
              env = [
                {
                  name = "TZ";
                  value = timezone;
                }
                {
                  # Non-privileged port so it binds fine as non-root (994).
                  name = "PORT";
                  value = "13378";
                }
              ];
              ports.http.containerPort = 13378;
              volumeMounts = [
                {
                  name = "audiobooks";
                  mountPath = "/audiobooks";
                }
                {
                  name = "config";
                  mountPath = "/config";
                }
                {
                  name = "metadata";
                  mountPath = "/metadata";
                }
              ];
            };
            volumes = [
              {
                name = "audiobooks";
                hostPath = {
                  path = "${mediaRoot}/audiobooks";
                  type = "Directory";
                };
              }
              {
                name = "config";
                hostPath = {
                  path = "${mediaRoot}/apps/audiobookshelf/config";
                  type = "Directory";
                };
              }
              {
                name = "metadata";
                hostPath = {
                  path = "${mediaRoot}/apps/audiobookshelf/metadata";
                  type = "Directory";
                };
              }
            ];
          };
        };
      };
      services = l.mkService {
        name = "audiobookshelf";
        port = 13378;
      };
      ingresses = l.mkIngress {
        name = "audiobookshelf";
        port = 13378;
        host = "audiobooks${ingressSuffix}";
      };
    };
  };
}

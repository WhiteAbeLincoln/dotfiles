# Calibre-Web-Automated: the EPUB/PDF library, replacing native calibre-web. A
# LinuxServer-lineage image, so it uses the shared mkLsioApp helper (root->994
# via PUID/PGID, fsGroup, Recreate). Reads the EXISTING Calibre library at
# ${mediaRoot}/books in place (same format + metadata.db as the old service);
# /cwa-book-ingest is CWA's BookDrop auto-import/convert folder. Local accounts
# now; native OIDC wired to Authelia later. See the design spec.
{config, ...}: let
  ingressSuffix = config.homelab.ingressSuffix;
  mediaRoot = config.homelab.media.root;
  mediaUid = config.users.users._media.uid;
  timezone = config.time.timeZone;
in {
  services.k3s.workloads.module = {
    lib,
    k8sLib,
    ...
  }: let
    l = k8sLib;
  in {
    applications = lib.mkMerge [
      {
        # The ebook/audiobook apps live in their own namespace with the same boundary
        # the `media` (torrent/arr) namespace has: default-deny-ingress, re-opened only
        # for intra-namespace traffic and the Traefik ingress controller (kube-system).
        # This keeps these apps isolated from the torrent stack.
        library-network = {
          namespace = "library";
          createNamespace = true;
          yamls = [
            (builtins.toJSON {
              apiVersion = "networking.k8s.io/v1";
              kind = "NetworkPolicy";
              metadata = {
                name = "allow-intra-and-ingress";
                namespace = "library";
              };
              spec = {
                podSelector = {};
                policyTypes = ["Ingress"];
                ingress = [
                  {from = [{podSelector = {};}];}
                  {
                    from = [
                      {
                        namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                      }
                    ];
                  }
                ];
              };
            })
          ];
        };
      }
      (l.mkLsioApp {
        name = "calibre-web-automated";
        image = "ghcr.io/crocodilestick/calibre-web-automated:v4.0.6@sha256:c31a738b6d5ec6982c050063dd3f063b6943eb1051fc81144789f840d9093a8d";
        port = 8083;
        namespace = "library";
        host = "books${ingressSuffix}";
        configPath = "${mediaRoot}/apps/calibre-web-automated/config";
        inherit ingressSuffix mediaUid timezone;
        extraVolumes = [
          {
            name = "calibre-library";
            hostPath = {
              path = "${mediaRoot}/books";
              type = "Directory";
            };
          }
          {
            name = "ingest";
            hostPath = {
              path = "${mediaRoot}/apps/calibre-web-automated/ingest";
              type = "Directory";
            };
          }
        ];
        extraMounts = [
          {
            name = "calibre-library";
            mountPath = "/calibre-library";
          }
          {
            name = "ingest";
            mountPath = "/cwa-book-ingest";
          }
        ];
      })
    ];
  };
}

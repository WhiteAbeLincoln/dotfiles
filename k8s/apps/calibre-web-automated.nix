# Calibre-Web-Automated: the EPUB/PDF library, replacing native calibre-web. A
# LinuxServer-lineage image, so it uses the shared mkLsioApp helper (root->994
# via PUID/PGID, fsGroup, Recreate). Reads the EXISTING Calibre library at
# ${mediaRoot}/books in place (same format + metadata.db as the old service);
# /cwa-book-ingest is CWA's BookDrop auto-import/convert folder. Local accounts
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
in {
  applications = l.mkLsioApp {
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
  };
}

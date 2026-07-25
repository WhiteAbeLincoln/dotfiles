# prowlarr / radarr / sonarr migrated off docker onto k3s. Each keeps its
# existing /config dir in place (hostPath, no data copy) and 994 uid/gid. radarr
# and sonarr also mount the shared /data/Media library (matching their current
# docker `/data` mapping); prowlarr manages only indexers, so it needs no library.
{
  lib,
  ingressSuffix,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  facts = {inherit ingressSuffix mediaRoot mediaUid timezone;};
  # Shared media-library mount for the apps that manage the library.
  mediaVolume = {
    name = "media";
    hostPath = {
      path = mediaRoot;
      type = "Directory";
    };
  };
  mediaMount = {
    name = "media";
    mountPath = "/data";
  };
  forwardAuth = {
    "traefik.ingress.kubernetes.io/router.middlewares" = "media-forward-auth@kubernetescrd";
  };
in {
  applications = lib.mkMerge [
    (l.mkLsioApp (facts
      // {
        name = "prowlarr";
        image = "lscr.io/linuxserver/prowlarr@sha256:2f3d31307beba3ba2dd226d191f5f5c14ee3b4d8b49277c64683f5ed97083179";
        port = 9696;
        configPath = "${mediaRoot}/docker-services/torrent-config/prowlarr";
        ingressAnnotations = forwardAuth;
      }))
    (l.mkLsioApp (facts
      // {
        name = "radarr";
        image = "lscr.io/linuxserver/radarr@sha256:e35056574cdc695a9ee745aa1ecda9eab3842450bf4b7b8471b023790fa3861d";
        port = 7878;
        configPath = "${mediaRoot}/docker-services/torrent-config/radarr";
        ingressAnnotations = forwardAuth;
        extraVolumes = [mediaVolume];
        extraMounts = [mediaMount];
      }))
    (l.mkLsioApp (facts
      // {
        name = "sonarr";
        image = "lscr.io/linuxserver/sonarr@sha256:24acea2956a0ccb11f103877d9f4f8576600fb34bff34820ed749c2256dab89f";
        port = 8989;
        configPath = "${mediaRoot}/docker-services/torrent-config/sonarr";
        ingressAnnotations = forwardAuth;
        extraVolumes = [mediaVolume];
        extraMounts = [mediaMount];
      }))
  ];
}

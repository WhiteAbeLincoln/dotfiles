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
in {
  applications = lib.mkMerge [
    (l.mkLsioApp (facts
      // {
        name = "prowlarr";
        image = "lscr.io/linuxserver/prowlarr:latest";
        port = 9696;
        configPath = "${mediaRoot}/docker-services/torrent-config/prowlarr";
      }))
    (l.mkLsioApp (facts
      // {
        name = "radarr";
        image = "lscr.io/linuxserver/radarr:latest";
        port = 7878;
        configPath = "${mediaRoot}/docker-services/torrent-config/radarr";
        extraVolumes = [mediaVolume];
        extraMounts = [mediaMount];
      }))
    (l.mkLsioApp (facts
      // {
        name = "sonarr";
        image = "lscr.io/linuxserver/sonarr:latest";
        port = 8989;
        configPath = "${mediaRoot}/docker-services/torrent-config/sonarr";
        extraVolumes = [mediaVolume];
        extraMounts = [mediaMount];
      }))
  ];
}

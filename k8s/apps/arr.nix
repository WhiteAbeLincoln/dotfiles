# prowlarr / radarr / sonarr migrated off docker onto k3s. Each keeps its
# existing /config dir in place (hostPath, no data copy) and 994 uid/gid. radarr
# and sonarr also mount the shared /data/Media library (matching their current
# docker `/data` mapping); prowlarr manages only indexers, so it needs no library.
{
  lib,
  ingressSuffix,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  # Shared /data/Media mount for the apps that manage the library.
  mediaVolume = {
    name = "media";
    hostPath = {
      path = "/data/Media";
      type = "Directory";
    };
  };
  mediaMount = {
    name = "media";
    mountPath = "/data";
  };
in {
  applications = lib.mkMerge [
    (l.mkArrApp {
      name = "prowlarr";
      image = "lscr.io/linuxserver/prowlarr:latest";
      port = 9696;
      inherit ingressSuffix;
    })
    (l.mkArrApp {
      name = "radarr";
      image = "lscr.io/linuxserver/radarr:latest";
      port = 7878;
      inherit ingressSuffix;
      extraVolumes = [mediaVolume];
      extraMounts = [mediaMount];
    })
    (l.mkArrApp {
      name = "sonarr";
      image = "lscr.io/linuxserver/sonarr:latest";
      port = 8989;
      inherit ingressSuffix;
      extraVolumes = [mediaVolume];
      extraMounts = [mediaMount];
    })
  ];
}

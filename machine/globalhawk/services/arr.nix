# prowlarr / radarr / sonarr migrated off docker onto k3s. Each keeps its
# existing /config dir in place (hostPath, no data copy) and 994 uid/gid. radarr
# and sonarr also mount the shared /data/Media library (matching their current
# docker `/data` mapping); prowlarr manages only indexers, so it needs no library.
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
          image = "lscr.io/linuxserver/prowlarr@sha256:2f3d31307beba3ba2dd226d191f5f5c14ee3b4d8b49277c64683f5ed97083179";
          port = 9696;
          configPath = "${mediaRoot}/docker-services/torrent-config/prowlarr";
          forwardAuth = true;
        }))
      (l.mkLsioApp (facts
        // {
          name = "radarr";
          image = "lscr.io/linuxserver/radarr@sha256:e35056574cdc695a9ee745aa1ecda9eab3842450bf4b7b8471b023790fa3861d";
          port = 7878;
          configPath = "${mediaRoot}/docker-services/torrent-config/radarr";
          forwardAuth = true;
          extraVolumes = [mediaVolume];
          extraMounts = [mediaMount];
        }))
      (l.mkLsioApp (facts
        // {
          name = "sonarr";
          image = "lscr.io/linuxserver/sonarr@sha256:24acea2956a0ccb11f103877d9f4f8576600fb34bff34820ed749c2256dab89f";
          port = 8989;
          configPath = "${mediaRoot}/docker-services/torrent-config/sonarr";
          forwardAuth = true;
          extraVolumes = [mediaVolume];
          extraMounts = [mediaMount];
        }))

      # The torrent/arr stack lives in one namespace with a real boundary: nothing
      # outside `media` may open connections into it EXCEPT the ingress controller
      # (Traefik, in kube-system). Intra-namespace traffic (arr <-> qbittorrent) is
      # allowed. Egress is unrestricted (arr reaches indexers/trackers/DNS freely).
      # This is the isolation the flat docker `torrent` bridge never had.
      {
        media-network = {
          namespace = "media";
          createNamespace = true;
          yamls = [
            # Selecting all pods with an Ingress policy makes the namespace
            # default-deny-ingress; the rules below re-open only the intended sources.
            (builtins.toJSON {
              apiVersion = "networking.k8s.io/v1";
              kind = "NetworkPolicy";
              metadata = {
                name = "allow-intra-and-ingress";
                namespace = "media";
              };
              spec = {
                podSelector = {};
                policyTypes = ["Ingress"];
                ingress = [
                  # intra-namespace (arr <-> qbittorrent)
                  {from = [{podSelector = {};}];}
                  # the ingress controller (Traefik runs in kube-system)
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
    ];
  };
}

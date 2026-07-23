# The torrent/arr stack lives in one namespace with a real boundary: nothing
# outside `media` may open connections into it EXCEPT the ingress controller
# (Traefik, in kube-system). Intra-namespace traffic (arr <-> qbittorrent) is
# allowed. Egress is unrestricted (arr reaches indexers/trackers/DNS freely).
# This is the isolation the flat docker `torrent` bridge never had.
{...}: {
  applications.media-network = {
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

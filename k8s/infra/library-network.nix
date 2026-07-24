# The ebook/audiobook apps live in their own namespace with the same boundary
# the `media` (torrent/arr) namespace has: default-deny-ingress, re-opened only
# for intra-namespace traffic and the Traefik ingress controller (kube-system).
# This keeps these apps isolated from the torrent stack.
{...}: {
  applications.library-network = {
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

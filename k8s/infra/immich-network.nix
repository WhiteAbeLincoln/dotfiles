# Immich lives in its own namespace with the same boundary as media/library:
# default-deny-ingress, re-opened only for intra-namespace traffic (server <->
# postgres/redis/ML) and the Traefik ingress controller (kube-system).
{...}: {
  applications.immich-network = {
    namespace = "immich";
    createNamespace = true;
    yamls = [
      (builtins.toJSON {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "allow-intra-and-ingress";
          namespace = "immich";
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

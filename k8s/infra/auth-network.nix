# Authelia lives in its own namespace with the same boundary as media/immich:
# default-deny-ingress, re-opened only for intra-namespace traffic and the
# Traefik ingress controller (kube-system). Traefik is the sole external source
# and it fronts the portal, the forward-auth callback, AND the OIDC endpoints,
# so no other cross-namespace ingress is needed.
{...}: {
  applications.auth-network = {
    namespace = "auth";
    createNamespace = true;
    yamls = [
      (builtins.toJSON {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "allow-intra-and-ingress";
          namespace = "auth";
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

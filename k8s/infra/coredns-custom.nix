# OIDC relying parties must resolve Authelia's canonical HTTPS issuer from
# inside the cluster. The name is LAN-private in AdGuard, while k3s CoreDNS
# forwards through the host's non-AdGuard resolver. Rewrite it to Traefik's
# cluster Service; an exact CoreDNS rewrite also rewrites the answer name back,
# so clients still see and validate the canonical TLS/issuer hostname.
{ingressSuffix, ...}: {
  applications.coredns-custom = {
    namespace = "kube-system";
    createNamespace = false;
    resources.configMaps.coredns-custom.data."private-ingress.override" = ''
      rewrite name exact auth${ingressSuffix} traefik.kube-system.svc.cluster.local
    '';
  };
}

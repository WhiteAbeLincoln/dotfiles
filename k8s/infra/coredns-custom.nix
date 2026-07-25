# OIDC relying parties must resolve Authelia's canonical HTTPS issuer from
# inside the cluster. The name is LAN-private in AdGuard, while k3s CoreDNS
# forwards through the host's non-AdGuard resolver. k3s already mounts the
# optional coredns-custom ConfigMap and imports every `*.override` key.
{
  hostGatewayIp,
  ingressSuffix,
  ...
}: {
  applications.coredns-custom = {
    namespace = "kube-system";
    createNamespace = false;
    resources.configMaps.coredns-custom.data."private-ingress.override" = ''
      hosts {
        ${hostGatewayIp} auth${ingressSuffix}
        fallthrough
      }
    '';
  };
}

# Plex stays NATIVE (GPU /dev/dri, plex.tv account, native clients that can't do
# forward-auth). We only give it a hostname + TLS + a row in the same routing
# table as everything else, per the design's host<->cluster boundary. Traefik
# routes Ingress -> Service -> manual Endpoints -> the host's Plex on :32400.
{
  lib,
  ingressSuffix,
  ...
}: let
  host = "plex${ingressSuffix}";
  # The host as seen from a pod: every pod's default gateway is the node's
  # flannel bridge (cni0 = <podCIDR>.1). Stable across DHCP lease changes (it's a
  # k3s cluster-CIDR constant, not the LAN IP), and cni0 is a trusted firewall
  # interface. Plex listens on 0.0.0.0:32400 so it answers here.
  hostGatewayIp = "10.42.0.1";
  plexPort = 32400;
in {
  applications.plex = {
    namespace = "plex";
    createNamespace = true;
    resources = {
      # Selector-less Service: its Endpoints are managed by hand (below), not by
      # a pod selector, so it can front an off-cluster backend.
      services.plex.spec.ports.web = {
        port = plexPort;
        targetPort = plexPort;
      };
      ingresses.plex = {
        metadata.annotations."cert-manager.io/cluster-issuer" = "globalhawk-ca";
        spec = {
          ingressClassName = "traefik";
          tls = [
            {
              hosts = [host];
              secretName = "plex-tls";
            }
          ];
          rules = [
            {
              inherit host;
              http.paths = [
                {
                  path = "/";
                  pathType = "Prefix";
                  backend.service = {
                    name = "plex";
                    port.number = plexPort;
                  };
                }
              ];
            }
          ];
        };
      };
    };
    # EndpointSlice (the non-deprecated replacement for Endpoints) binds the
    # selector-less Service to the off-cluster backend. The
    # kubernetes.io/service-name label links it to the `plex` Service; the port
    # name "web" matches the Service port. Authored raw (JSON is valid YAML).
    yamls = [
      (builtins.toJSON {
        apiVersion = "discovery.k8s.io/v1";
        kind = "EndpointSlice";
        metadata = {
          name = "plex";
          namespace = "plex";
          labels."kubernetes.io/service-name" = "plex";
        };
        addressType = "IPv4";
        endpoints = [{addresses = [hostGatewayIp];}];
        ports = [
          {
            name = "web";
            port = plexPort;
            protocol = "TCP";
          }
        ];
      })
    ];
  };
}

# Plex stays NATIVE (GPU /dev/dri, plex.tv account, native clients that can't do
# forward-auth). We only give it a hostname + TLS + a row in the same routing
# table as everything else, per the design's host<->cluster boundary. Traefik
# routes Ingress -> Service -> manual Endpoints -> the host's Plex on :32400.
{
  lib,
  ingressSuffix,
  hostGatewayIp,
  ...
}: let
  host = "plex${ingressSuffix}";
  # hostGatewayIp = the host as seen from a pod (flannel cni0 bridge), pinned in
  # machine/globalhawk/cluster-net.nix. Stable across DHCP lease changes and
  # rebuilds; cni0 is a trusted firewall interface; Plex listens on 0.0.0.0:32400.
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
        spec = {
          ingressClassName = "traefik";
          tls = [{hosts = [host];}];
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

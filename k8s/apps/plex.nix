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
  # globalhawk's LAN InternalIP. DHCP-assigned today; if it churns, update here
  # (or give the box a static reservation). Not secret (RFC1918).
  nodeIp = "192.168.1.197";
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
    # Endpoints has no typed nixidy option here and must name-match the Service;
    # authored raw (JSON is valid YAML). Port name "web" matches the Service port.
    yamls = [
      (builtins.toJSON {
        apiVersion = "v1";
        kind = "Endpoints";
        metadata = {
          name = "plex";
          namespace = "plex";
        };
        subsets = [
          {
            addresses = [{ip = nodeIp;}];
            ports = [
              {
                name = "web";
                port = plexPort;
              }
            ];
          }
        ];
      })
    ];
  };
}

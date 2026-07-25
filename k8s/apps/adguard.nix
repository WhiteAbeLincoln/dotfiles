# AdGuard Home stays host-native (machine/globalhawk/adguard.nix) and is SSO-
# EXCEPTED (no proxy-header trust -> forward-auth would only double-login one
# admin page). We only give it a hostname + TLS + a routing-table row, exactly
# like Plex: Traefik -> selector-less Service -> manual EndpointSlice -> the
# host's AdGuard web UI on :3000 (reachable from pods over the trusted cni0
# bridge; no firewall change). AdGuard's own admin login remains the gate.
{
  lib,
  ingressSuffix,
  hostGatewayIp,
  ...
}: let
  host = "adguard${ingressSuffix}";
  port = 3000;
in {
  applications.adguard = {
    namespace = "adguard";
    createNamespace = true;
    resources = {
      services.adguard.spec.ports.web = {
        inherit port;
        targetPort = port;
      };
      ingresses.adguard.spec = {
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
                  name = "adguard";
                  port.number = port;
                };
              }
            ];
          }
        ];
      };
    };
    yamls = [
      (builtins.toJSON {
        apiVersion = "discovery.k8s.io/v1";
        kind = "EndpointSlice";
        metadata = {
          name = "adguard";
          namespace = "adguard";
          labels."kubernetes.io/service-name" = "adguard";
        };
        addressType = "IPv4";
        endpoints = [{addresses = [hostGatewayIp];}];
        ports = [
          {
            name = "web";
            port = port;
            protocol = "TCP";
          }
        ];
      })
    ];
  };
}

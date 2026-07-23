# Phase-0 end-to-end proof: a stateless echo server. Exists only to validate the
# author->render->deliver->apply->reach chain; removed once real apps are migrated.
{
  lib,
  ingressSuffix,
  ...
}: let
  labels = (import ../lib.nix {inherit lib;}).appLabels "whoami";
  host = "whoami${ingressSuffix}";
in {
  applications.whoami = {
    namespace = "whoami";
    createNamespace = true;
    resources = {
      deployments.whoami.spec = {
        replicas = 1;
        selector.matchLabels = labels;
        template = {
          metadata.labels = labels;
          spec.containers.whoami = {
            image = "traefik/whoami:v1.10.2";
            ports.http.containerPort = 80;
          };
        };
      };
      services.whoami.spec = {
        selector = labels;
        ports.http = {
          port = 80;
          targetPort = 80;
        };
      };
      ingresses.whoami = {
        metadata.annotations."cert-manager.io/cluster-issuer" = "globalhawk-ca";
        spec = {
          ingressClassName = "traefik";
          tls = [
            {
              hosts = [host];
              secretName = "whoami-tls";
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
                    name = "whoami";
                    port.number = 80;
                  };
                }
              ];
            }
          ];
        };
      };
    };
  };
}

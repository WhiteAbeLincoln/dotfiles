# Stateless echo server kept as a debugging canary: enable its import in
# k8s/default.nix to test the ingress + cert-manager + routing path in isolation
# (is a problem infra-wide or app-specific?) without touching a real service.
# Disabled by default. Originally the Phase-0 end-to-end proof of the
# author->render->deliver->apply->reach chain.
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

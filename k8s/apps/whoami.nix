# Phase-0 end-to-end proof: a stateless echo server. Exists only to validate the
# author->render->deliver->apply->reach chain; removed once real apps are migrated.
{lib, ...}: let
  labels = (import ../lib.nix {inherit lib;}).appLabels "whoami";
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
    };
  };
}

{
  inputs,
  pkgs,
}: let
  lib = pkgs.lib;
  evaluated = lib.evalModules {
    specialArgs = {inherit inputs pkgs;};
    modules = [
      ../../modules/nixos/k3s-workloads.nix
      {
        options = {
          assertions = lib.mkOption {
            type = lib.types.listOf lib.types.anything;
            default = [];
          };
          services.k3s.enable = lib.mkOption {type = lib.types.bool;};
          services.k3s.manifests = lib.mkOption {
            type = lib.types.attrsOf lib.types.anything;
            default = {};
          };
          services.fixture.port = lib.mkOption {type = lib.types.port;};
        };
        config = {
          services.k3s.enable = true;
          services.fixture.port = 4321;
          services.k3s.workloads = {
            enable = true;
            module = {lib, ...}: {
              nixidy.target.repository = "file:///dev/null";
              nixidy.target.branch = "main";
              applications.fixture = {
                namespace = "default";
                resources = {
                  deployments.fixture.spec = {
                    replicas = lib.mkDefault 1;
                    selector.matchLabels.app = "fixture";
                    template = {
                      metadata.labels.app = "fixture";
                      spec.containers.fixture.image = "registry.invalid/fixture:test";
                    };
                  };
                  services.fixture.spec = {
                    selector.app = "fixture";
                    ports.http = {
                      port = 80;
                      targetPort = lib.mkDefault 80;
                    };
                  };
                };
              };
            };
          };
        };
      }
      ({lib, ...}: {
        services.k3s.workloads.module = {
          applications.fixture.resources.deployments.fixture.spec.replicas =
            lib.mkForce 2;
        };
      })
      ({config, ...}: {
        services.k3s.workloads.module = {nixosConfig, ...}: {
          applications.fixture.resources.services.fixture.spec.ports.http.targetPort =
            nixosConfig.services.fixture.port;
        };
      })
    ];
  };
  workload = evaluated.config.services.k3s.workloads;
  service =
    workload.evaluatedConfig.applications.fixture.resources.services.fixture;
in
  assert lib.assertMsg
  (lib.all (assertion: assertion.assertion) evaluated.config.assertions)
  (lib.concatMapStringsSep "\n" (assertion: assertion.message) evaluated.config.assertions);
  assert workload.evaluatedConfig.applications.fixture.resources.deployments.fixture.spec.replicas == 2;
  assert (builtins.head service.spec.ports).targetPort == 4321;
    pkgs.runCommand "k3s-workloads-module-test" {} ''
      service="$(find -L ${workload.renderedPackage}/fixture -name 'Service-fixture.yaml' -print -quit)"
      test -n "$service"
      test "$(${pkgs.yq-go}/bin/yq '.spec.ports[0].targetPort' "$service")" = 4321
      touch "$out"
    ''

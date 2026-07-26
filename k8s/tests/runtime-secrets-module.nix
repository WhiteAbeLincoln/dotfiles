{
  inputs,
  pkgs,
}: let
  lib = pkgs.lib;
  evaluate = runtimeSecrets:
    lib.evalModules {
      specialArgs = {inherit inputs pkgs;};
      modules = [
        ../../modules/nixos/k3s-runtime-secrets.nix
        {
          options = {
            assertions = lib.mkOption {
              type = lib.types.listOf lib.types.anything;
              default = [];
            };
            sops.secrets = lib.mkOption {type = lib.types.attrsOf lib.types.anything;};
            sops.placeholder = lib.mkOption {type = lib.types.attrsOf lib.types.str;};
            sops.templates = lib.mkOption {
              type = lib.types.attrsOf (lib.types.submodule {
                options = {
                  path = lib.mkOption {type = lib.types.str;};
                  mode = lib.mkOption {type = lib.types.str;};
                  owner = lib.mkOption {type = lib.types.str;};
                  content = lib.mkOption {type = lib.types.lines;};
                };
              });
              default = {};
            };
          };
          config = {
            sops.secrets = {
              scalar = {};
              encoded = {};
            };
            sops.placeholder = {
              scalar = "<scalar-placeholder>";
              encoded = "<encoded-placeholder>";
            };
            services.k3s.runtimeSecrets = runtimeSecrets;
          };
        }
      ];
    };
  validSecret = {
    fixture = {
      namespace = "testing";
      stringData.password.sopsSecret = "scalar";
      data."payload.bin".sopsSecret = "encoded";
    };
    cloudflare-api-token = {
      namespace = "cert-manager";
      manifestStem = "cloudflare-token";
      stringData.api-token.sopsSecret = "scalar";
    };
    independent = {
      namespace = "other";
      stringData.token.sopsSecret = "scalar";
    };
  };
  evaluated = evaluate validSecret;
  failedAssertion = lib.findFirst (item: !item.assertion) null evaluated.config.assertions;
  template = evaluated.config.sops.templates."k3s-fixture.yaml";
  manifest = builtins.fromJSON template.content;
  cloudflareTemplate = evaluated.config.sops.templates."k3s-cloudflare-token.yaml";
  cloudflareManifest = builtins.fromJSON cloudflareTemplate.content;
  independentManifest =
    builtins.fromJSON evaluated.config.sops.templates."k3s-independent.yaml".content;
  validationCases = [
    {
      runtimeSecrets."Invalid_Name" = validSecret.fixture;
      expected = "services.k3s.runtimeSecrets.Invalid_Name: invalid Kubernetes Secret name";
    }
    {
      runtimeSecrets.fixture = validSecret.fixture // {namespace = "Invalid_Namespace";};
      expected = "services.k3s.runtimeSecrets.fixture: invalid namespace";
    }
    {
      runtimeSecrets.fixture =
        validSecret.fixture
        // {
          data =
            validSecret.fixture.data
            // {
              password.sopsSecret = "encoded";
            };
        };
      expected = "services.k3s.runtimeSecrets.fixture: a key cannot appear in both stringData and data";
    }
    {
      runtimeSecrets.fixture =
        validSecret.fixture
        // {
          stringData = {
            "invalid/key".sopsSecret = "scalar";
          };
        };
      expected = "services.k3s.runtimeSecrets.fixture: invalid Kubernetes Secret key";
    }
    {
      runtimeSecrets.fixture =
        validSecret.fixture
        // {
          stringData.password.sopsSecret = "undeclared";
        };
      expected = "services.k3s.runtimeSecrets.fixture: references an undeclared sops secret";
    }
    {
      runtimeSecrets.fixture = validSecret.fixture // {manifestStem = "../fixture";};
      expected = "services.k3s.runtimeSecrets.fixture: invalid manifest stem";
    }
    {
      runtimeSecrets = {
        first = validSecret.independent // {manifestStem = "shared";};
        second = validSecret.independent // {manifestStem = "shared";};
      };
      expected = "services.k3s.runtimeSecrets: manifest stems must be unique";
    }
  ];
  validationCasePasses = case: let
    result = evaluate case.runtimeSecrets;
    failure = lib.findFirst (item: !item.assertion) null result.config.assertions;
  in
    failure != null && failure.message == case.expected;
in
  if failedAssertion != null
  then throw failedAssertion.message
  else
    assert lib.all validationCasePasses validationCases;
    assert template.path == "/var/lib/rancher/k3s/server/manifests/sops-fixture.yaml";
    assert template.mode == "0400";
    assert template.owner == "root";
    assert manifest.metadata.name == "fixture";
    assert manifest.metadata.namespace == "testing";
    assert manifest.stringData.password == "<scalar-placeholder>";
    assert manifest.data."payload.bin" == "<encoded-placeholder>";
    assert cloudflareTemplate.path == "/var/lib/rancher/k3s/server/manifests/sops-cloudflare-token.yaml";
    assert cloudflareManifest.metadata.name == "cloudflare-api-token";
    assert cloudflareManifest.metadata.namespace == "cert-manager";
    assert cloudflareManifest.stringData.api-token == "<scalar-placeholder>";
    assert independentManifest.metadata.name == "independent";
    assert independentManifest.metadata.namespace == "other";
    assert independentManifest.stringData.token == "<scalar-placeholder>";
      pkgs.runCommand "k3s-runtime-secrets-module-test" {} ''
        touch "$out"
      ''

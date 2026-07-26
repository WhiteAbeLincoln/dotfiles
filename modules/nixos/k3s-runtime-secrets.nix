{
  config,
  lib,
  ...
}: let
  cfg = config.services.k3s.runtimeSecrets;
  valueType = lib.types.submodule {
    options.sopsSecret = lib.mkOption {
      type = lib.types.str;
      description = "Declared sops.secrets key substituted at activation.";
    };
  };
  secretType = lib.types.submodule ({name, ...}: {
    options = {
      namespace = lib.mkOption {type = lib.types.str;};
      manifestStem = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Filename stem for the root-only k3s manifest.";
      };
      stringData = lib.mkOption {
        type = lib.types.attrsOf valueType;
        default = {};
      };
      data = lib.mkOption {
        type = lib.types.attrsOf valueType;
        default = {};
      };
    };
  });
  dnsLabel = value:
    lib.stringLength value
    <= 63
    && builtins.match "[a-z0-9]([-a-z0-9]*[a-z0-9])?" value != null;
  keyValid = value:
    lib.stringLength value
    <= 253
    && builtins.match "[-._a-zA-Z0-9]+" value != null;
  refs = values: lib.mapAttrs (_: value: config.sops.placeholder.${value.sopsSecret}) values;
  templateFor = name: secret: {
    name = "k3s-${secret.manifestStem}.yaml";
    value = {
      path = "/var/lib/rancher/k3s/server/manifests/sops-${secret.manifestStem}.yaml";
      mode = "0400";
      owner = "root";
      content = builtins.toJSON {
        apiVersion = "v1";
        kind = "Secret";
        metadata = {
          inherit name;
          inherit (secret) namespace;
        };
        type = "Opaque";
        stringData = refs secret.stringData;
        data = refs secret.data;
      };
    };
  };
in {
  options.services.k3s.runtimeSecrets = lib.mkOption {
    type = lib.types.attrsOf secretType;
    default = {};
  };

  config = {
    assertions =
      lib.flatten (lib.mapAttrsToList (name: secret: [
          {
            assertion = dnsLabel name;
            message = "services.k3s.runtimeSecrets.${name}: invalid Kubernetes Secret name";
          }
          {
            assertion = dnsLabel secret.namespace;
            message = "services.k3s.runtimeSecrets.${name}: invalid namespace";
          }
          {
            assertion = dnsLabel secret.manifestStem;
            message = "services.k3s.runtimeSecrets.${name}: invalid manifest stem";
          }
          {
            assertion =
              lib.intersectLists (lib.attrNames secret.stringData) (lib.attrNames secret.data) == [];
            message = "services.k3s.runtimeSecrets.${name}: a key cannot appear in both stringData and data";
          }
          {
            assertion = lib.all keyValid (lib.attrNames (secret.stringData // secret.data));
            message = "services.k3s.runtimeSecrets.${name}: invalid Kubernetes Secret key";
          }
          {
            assertion =
              lib.all
              (value: builtins.hasAttr value.sopsSecret config.sops.secrets)
              (lib.attrValues (secret.stringData // secret.data));
            message = "services.k3s.runtimeSecrets.${name}: references an undeclared sops secret";
          }
        ])
        cfg)
      ++ [
        {
          assertion =
            lib.length (lib.unique (lib.mapAttrsToList (_: secret: secret.manifestStem) cfg))
            == lib.length (lib.attrNames cfg);
          message = "services.k3s.runtimeSecrets: manifest stems must be unique";
        }
      ];

    sops.templates = builtins.listToAttrs (lib.mapAttrsToList templateFor cfg);
  };
}

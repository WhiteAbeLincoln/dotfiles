{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.k3s.workloads;
  rendered = inputs.nixidy.lib.mkEnv {
    inherit pkgs;
    extraSpecialArgs.nixosConfig = config;
    modules = [
      {
        nixidy.env = "globalhawk";
        nixidy.target.rootPath = "./manifests/globalhawk";
      }
      cfg.module
    ];
  };
  combinedManifest = pkgs.runCommand "k3s-workloads-globalhawk.yaml" {} ''
    : > "$out"
    for file in $(${pkgs.findutils}/bin/find -L ${rendered.environmentPackage} \
      -name '*.yaml' -not -path '*/apps/*' | sort); do
      cat "$file" >> "$out"
      printf '\n---\n' >> "$out"
    done
  '';
in {
  options.services.k3s.workloads = {
    enable = lib.mkEnableOption "Nix-authored Kubernetes workloads rendered into the k3s auto-deploy lane";

    module = lib.mkOption {
      type = lib.types.deferredModule;
      default = {};
      description = "Workload-renderer module merged from NixOS service modules.";
    };

    evaluatedConfig = lib.mkOption {
      type = lib.types.raw;
      readOnly = true;
      description = "Evaluated workload-renderer configuration.";
    };

    renderedPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = "Directory containing the rendered Kubernetes manifests.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.k3s.enable;
        message = "services.k3s.workloads requires services.k3s.enable";
      }
    ];

    services.k3s.workloads = {
      evaluatedConfig = rendered.config;
      renderedPackage = rendered.environmentPackage;
    };

    # Keep this exact Addon key: changing it changes k3s object ownership.
    services.k3s.manifests.nixidy.source = combinedManifest;
  };
}

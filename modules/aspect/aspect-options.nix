{lib, ...}: let
  inherit (lib) mkOption types;
in {
  options = {
    nixos = mkOption {
      type = types.deferredModule;
      default = {};
      description = "NixOS module contributed by selected aspects.";
    };
    darwin = mkOption {
      type = types.deferredModule;
      default = {};
      description = "nix-darwin module contributed by selected aspects.";
    };
    homeManager = mkOption {
      type = types.deferredModule;
      default = {};
      description = "Home Manager module contributed by selected aspects.";
    };
  };
}

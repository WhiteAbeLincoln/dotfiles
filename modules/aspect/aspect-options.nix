{lib, ...}: let
  inherit (lib) mkOption types;
in {
  options = {
    nixpkgs = {
      overlays = mkOption {
        type = types.listOf types.raw;
        default = [];
        description = "Nixpkgs overlays contributed by selected aspects.";
      };
      config = mkOption {
        type = types.lazyAttrsOf types.raw;
        default = {};
        description = "Nixpkgs configuration contributed by selected aspects.";
      };
    };
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

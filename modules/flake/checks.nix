{
  config,
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ../aspect/lib.nix {inherit inputs lib self;};
  hosts = config.dotfiles.hosts;
  machineRootsOnly = lib.all (
    host:
      !(host ? stateVersion)
      && !(host ? modules)
      && !(host ? homeModules)
  ) (lib.attrValues hosts);
  testHome = constructors.mkConfiguration "aspect-test" {
    class = "homeManager";
    system = "x86_64-linux";
    hostName = "aspect-test";
    primaryUser = "tester";
    aspects = [
      ./tests/home-manager-aspect.nix
      {
        homeManager = {
          programs.aspect-constructor.enable = false;
        };
      }
    ];
  };
in {
  perSystem = {
    pkgs,
    system,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.machine-aspect-roots = assert machineRootsOnly;
        pkgs.runCommand "machine-aspect-roots" {} "touch $out";
      checks.home-manager-host-class = assert testHome.config.programs.aspect-constructor.enable == false;
      assert testHome.config.home.file.".aspect-constructor-test".text == "overridden";
      assert testHome.config.home.file.".aspect-nixpkgs-test".text == "overlay-applied";
        pkgs.runCommand "home-manager-host-class" {} "touch $out";
    };
}

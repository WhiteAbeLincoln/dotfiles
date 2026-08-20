{
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ./lib.nix {inherit inputs lib self;};
  testHome = constructors.mkConfiguration "aspect-test" {
    class = "homeManager";
    system = "x86_64-linux";
    hostName = "aspect-test";
    primaryUser = "tester";
    stateVersion = {
      system = null;
      home = "26.05";
    };
    aspects = [./tests/home-manager-aspect.nix];
    modules = [];
    homeModules = [{programs.aspect-constructor.enable = false;}];
  };
in {
  perSystem = {
    pkgs,
    system,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.home-manager-host-class = assert testHome.config.programs.aspect-constructor.enable == false;
      assert testHome.config.home.file.".aspect-constructor-test".text == "overridden";
        pkgs.runCommand "home-manager-host-class" {} "touch $out";
    };
}

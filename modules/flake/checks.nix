{
  config,
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ../aspect/constructors.nix {
    inherit inputs lib self;
    inherit (config.dotfiles) providers;
    defaultAspectsEnabled = true;
  };
  constructorsWithoutDefaults = import ../aspect/constructors.nix {
    inherit inputs lib self;
    inherit (config.dotfiles) providers;
    defaultAspectsEnabled = false;
  };
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
    user = "tester";
    aspects = [
      ./tests/home-manager-aspect.nix
      {
        homeManager = {
          programs.aspect-constructor.enable = false;
        };
      }
    ];
  };
  testHomeWithoutDefaults = constructorsWithoutDefaults.mkConfiguration "aspect-test-no-defaults" {
    class = "homeManager";
    system = "x86_64-linux";
    hostName = "aspect-test";
    user = "tester";
    aspects = [
      {
        homeManager = {config, ...}: {
          home.username = "custom-user";
          home.homeDirectory = "/tmp/custom-home";
          home.stateVersion = "26.05";
          home.file.".aspect-no-defaults".text = config.dotfiles.host.user;
        };
      }
    ];
  };
  portableFlake = inputs.flake-parts.lib.mkFlake {inherit inputs;} {
    imports = [../aspect];

    dotfiles = {
      sharedAspects = [];
      hosts.portable-test = {
        class = "homeManager";
        system = "x86_64-linux";
        hostName = "portable-test";
        user = "tester";
        aspects = [./tests/home-manager-aspect.nix];
      };
    };

    flake.homeConfigurations.unrelated = "sentinel";
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
      checks.portable-aspect-library = assert portableFlake.homeConfigurations.unrelated == "sentinel";
      assert portableFlake.homeConfigurations."tester@portable-test".config.dotfiles.host.hostName == "portable-test";
        pkgs.runCommand "portable-aspect-library" {} "touch $out";
      checks.home-manager-host-class = assert testHome.config.programs.aspect-constructor.enable == false;
      assert testHome.config.home.file.".aspect-constructor-test".text == "overridden";
      assert testHome.config.home.file.".aspect-nixpkgs-test".text == "overlay-applied";
      assert testHome.config.dotfiles.host
      == {
        class = "homeManager";
        system = "x86_64-linux";
        hostName = "aspect-test";
        user = "tester";
      };
      assert testHomeWithoutDefaults.config.dotfiles.host.user == "tester";
      assert testHomeWithoutDefaults.config.home.username == "custom-user";
      assert testHomeWithoutDefaults.config.home.file.".aspect-no-defaults".text == "tester";
        pkgs.runCommand "home-manager-host-class" {} "touch $out";
    };
}

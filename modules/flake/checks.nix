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
  testDarwin = constructors.mkConfiguration "aspect-darwin-test" {
    class = "darwin";
    system = "aarch64-darwin";
    hostName = "aspect-darwin-test";
    user = "tester";
    aspects = [
      {
        darwin.system.stateVersion = 5;
        homeManager.home.stateVersion = "26.05";
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
  portableFlakeWithHomeNameCollision = inputs.flake-parts.lib.mkFlake {inherit inputs;} {
    imports = [../aspect];

    dotfiles = {
      sharedAspects = [];
      hosts = {
        first = {
          class = "homeManager";
          system = "x86_64-linux";
          hostName = "collision";
          user = "tester";
          aspects = [];
        };
        second = {
          class = "homeManager";
          system = "x86_64-linux";
          hostName = "collision";
          user = "tester";
          aspects = [];
        };
      };
    };
  };
  collidingHomeOutputs = builtins.tryEval (
    builtins.attrNames portableFlakeWithHomeNameCollision.homeConfigurations
  );
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
      # Catches removal of generated-name uniqueness validation, which lets mapAttrs' silently discard a host.
      checks.portable-home-output-name-collisions = assert !collidingHomeOutputs.success;
        pkgs.runCommand "portable-home-output-name-collisions" {} "touch $out";
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
      checks.darwin-host-identity = assert testDarwin.config.networking.localHostName == "aspect-darwin-test";
      assert testDarwin.config.users.users.tester.home == "/Users/tester";
        pkgs.runCommand "darwin-host-identity" {} "touch $out";
    };
}

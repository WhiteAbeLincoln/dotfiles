{inputs, ...}: {
  dotfiles = {
    sharedAspects = [];
    extraSystems = ["x86_64-darwin"];
    hosts = {
      globalhawk = {
        class = "nixos";
        system = "x86_64-linux";
        primaryUser = "abe";
        # This value determines the NixOS release from which the default
        # settings for stateful data, like file locations and database versions
        # on your system were taken. It's perfectly fine and recommended to leave
        # this value at the release version of the first install of this system.
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        stateVersion = {
          system = "23.11"; # Did you read the comment?
          home = "23.11";
        };
        modules = [../../machine/globalhawk];
        homeModules = [../../machine/globalhawk/home.nix];
      };
      valkyrie = {
        class = "nixos";
        system = "x86_64-linux";
        primaryUser = "abe";
        stateVersion = {
          system = "26.05";
          home = "26.05";
        };
        modules = [
          inputs.determinate.nixosModules.default
          ../../machine/valkyrie
        ];
        homeModules = [../../machine/valkyrie/home.nix];
      };
      nighthawk = {
        class = "darwin";
        system = "aarch64-darwin";
        primaryUser = "abe";
        stateVersion = {
          system = 5;
          home = "24.05";
        };
        modules = [
          inputs.determinate.darwinModules.default
          ../../machine/nighthawk
        ];
        homeModules = [../../machine/nighthawk/home.nix];
      };
    };
  };
}

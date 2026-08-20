{...}: {
  dotfiles = {
    sharedAspects = [../../aspect/shared.nix];
    extraSystems = ["x86_64-darwin"];
    hosts = {
      globalhawk = {
        class = "nixos";
        system = "x86_64-linux";
        user = "abe";
        aspects = [../../machine/globalhawk];
      };
      valkyrie = {
        class = "nixos";
        system = "x86_64-linux";
        user = "abe";
        aspects = [../../machine/valkyrie];
      };
      nighthawk = {
        class = "darwin";
        system = "aarch64-darwin";
        user = "abe";
        aspects = [../../machine/nighthawk];
      };
    };
  };
}

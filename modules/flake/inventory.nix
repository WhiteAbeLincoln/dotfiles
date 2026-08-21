{...}: {
  dotfiles = {
    sharedAspects = [
      ../../aspect/shared.nix
      ({...}: {
        # TODO: aspect system still needs some work
        # I should be able to create 'aspect options' which
        # let me declare a value once and have it apply to different places
        # This seems similar to "quirks" from den
        nixos.time.timeZone = "America/Denver";
        darwin.time.timeZone = "America/Denver";
      })
    ];
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

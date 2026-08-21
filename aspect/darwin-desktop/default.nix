{...}: {
  imports = [../darwin-system.nix];
  darwin = {
    imports = [
      ./defaults-writer.nix
      ./module.nix
    ];
  };
}

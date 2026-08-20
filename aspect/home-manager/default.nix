{...}: {
  homeManager.imports = [./module.nix];
  nixos.home-manager.sharedModules = [./module.nix];
  darwin.home-manager.sharedModules = [./module.nix];
}

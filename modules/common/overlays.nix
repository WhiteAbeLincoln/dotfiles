{inputs, ...}: {
  nixpkgs.overlays = import ./overlay-list.nix {inherit inputs;};
  nixpkgs.config.allowUnfree = true;
}

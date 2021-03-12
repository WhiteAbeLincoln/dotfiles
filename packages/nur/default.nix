{
  nixpkgs.config.packageOverrides = pkgs: {
    nur = (import ./nur.nix) { inherit pkgs; };
  };
}
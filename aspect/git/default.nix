{...}: {
  homeManager = ./home.nix;
  darwin = {pkgs, ...}: {
    environment.systemPackages = [
      pkgs.git
      pkgs.git-crypt
    ];
  };
  nixos = {pkgs, ...}: {
    environment.systemPackages = [
      pkgs.git
      pkgs.git-crypt
    ];
  };
}

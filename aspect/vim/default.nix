{...}: {
  homeManager = ./home.nix;
  darwin = {pkgs, ...}: {
    environment.systemPackages = [
      pkgs.neovim
    ];
    environment.variables.EDITOR = "nvim";
  };
  nixos = {pkgs, ...}: {
    environment.systemPackages = [
      pkgs.neovim
    ];
    environment.variables.EDITOR = "nvim";
  };
}

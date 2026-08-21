{...}: {
  imports = [./fish ./git ./vim ./direnv ./starship ./modern-cli.nix];
  homeManager = {pkgs, ...}: {
    programs.nix-index.enable = true;
    home.packages = [
      pkgs.nil
    ];
  };
}

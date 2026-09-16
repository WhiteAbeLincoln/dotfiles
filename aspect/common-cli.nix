{...}: {
  imports = [
    ./fish
    ./git
    ./vim
    ./direnv
    ./starship
    ./modern-cli.nix
    ./nix-index.nix
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [
      pkgs.nil
    ];
  };
}

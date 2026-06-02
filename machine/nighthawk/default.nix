{
  pkgs,
  lib,
  config,
  ...
}: let
  user = config.meta.user;
in {
  imports = [
    ../../role/darwin.nix
    ../../program/direnv
  ];

  environment.systemPackages = [
    pkgs.git-crypt
    pkgs.vim
    # pkgs.bitwarden-cli
    pkgs.moonlight-qt
  ];
  environment.variables.EDITOR = "vim";
  environment.systemPath = ["/opt/homebrew/bin"];

  users.users.${user} = {
    description = "Abraham White";
    home = "/Users/${user}";
  };

  homebrew.enable = true;
  homebrew.brews = [];

  programs.fish.enable = true;

  system.stateVersion = 5;
}

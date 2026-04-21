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
    pkgs.vim
    # pkgs.bitwarden-cli
    pkgs.moonlight-qt
    pkgs.wezterm.terminfo
  ];
  environment.variables.EDITOR = "vim";
  environment.systemPath = ["/opt/homebrew/bin"];

  users.users.${user} = {
    description = "Abraham White";
    home = "/Users/${user}";
  };

  homebrew.enable = true;
  homebrew.brews = [
    # the nix packaged version fails to build, missing perl dependency for the gyp bindings
    "bitwarden-cli"
  ];

  programs.fish.enable = true;

  system.stateVersion = 5;
}

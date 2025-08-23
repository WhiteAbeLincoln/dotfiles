{
  pkgs,
  myUserName,
  lib,
  config,
  ...
}: {
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

  users.users.${myUserName} = {
    description = "Abraham White";
    home = "/Users/${myUserName}";
  };

  homebrew.enable = true;
  homebrew.brews = [
    # the nix packaged version fails to build, missing perl dependency for the gyp bindings
    "bitwarden-cli"
  ];

  programs.fish.enable = true;

  system.stateVersion = 5;
}

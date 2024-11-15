{ pkgs, myUserName, ... }:

{
  imports = [
    ../../role/darwin.nix
  ];

  environment.systemPackages = [
    pkgs.vim
    pkgs.bitwarden-cli
  ];
  environment.variables.EDITOR = "vim";

  users.users.${myUserName} = {
    description = "Abraham White";
    home = "/Users/${myUserName}";
  };

  homebrew.enable = true;
  homebrew.brews = [
  ];

  system.stateVersion = 5;
}

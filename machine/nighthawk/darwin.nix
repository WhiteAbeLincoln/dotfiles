{
  pkgs,
  lib,
  config,
  ...
}: let
  user = config.dotfiles.host.user;
in {
  networking.localHostName = "nighthawk";
  networking.computerName = "Abraham's MacBook Pro";

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
}

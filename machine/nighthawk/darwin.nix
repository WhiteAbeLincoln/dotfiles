{
  pkgs,
  lib,
  config,
  ...
}: let
  user = config.dotfiles.host.user;
in {
  networking.computerName = "Abraham's MacBook Pro";
  users.users.${user}.description = "Abraham White";

  environment.systemPackages = [
    # pkgs.bitwarden-cli
  ];
  environment.systemPath = ["/opt/homebrew/bin"];

  homebrew.enable = true;
  homebrew.brews = [];
}

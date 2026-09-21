{
  pkgs,
  config,
  ...
}: let
  user = config.dotfiles.host.user;
in {
  users.users.${user}.description = "Abraham White";

  environment.systemPackages = [
    pkgs.git
    pkgs.raycast
    # use gnu coreutils instead of macos
    pkgs.coreutils
  ];
  # environment.systemPath = ["/opt/homebrew/bin"];

  # homebrew.enable = true;
  # homebrew.brews = [];
}

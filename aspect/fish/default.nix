{lib, ...}: let
  systemModule = {
    config,
    pkgs,
    ...
  }: {
    programs.fish.enable = lib.mkDefault true;
    users.users.${config.dotfiles.host.user} = lib.mkIf config.programs.fish.enable {
      shell = lib.mkDefault pkgs.fish;
    };
  };
in {
  nixos = {config, ...}: {
    imports = [systemModule];
    # NixOS gives normal users a competing mkDefault shell via
    # users.defaultUserShell, so disable that fallback for the selected user.
    users.users.${config.dotfiles.host.user}.useDefaultShell = lib.mkIf config.programs.fish.enable false;
  };
  darwin = systemModule;
  homeManager = ./home.nix;
}

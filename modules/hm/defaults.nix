# HM defaults that used to live in make-cfg.nix's baseHmModule.
# home.stateVersion still belongs to each machine/*/home.nix because it varies.
{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.home-manager.enable = true;
  home.username = lib.mkDefault config.meta.user;
  home.homeDirectory = lib.mkDefault (
    if pkgs.stdenv.hostPlatform.isDarwin
    then "/Users/${config.meta.user}"
    else "/home/${config.meta.user}"
  );
}

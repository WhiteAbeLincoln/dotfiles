{ config, pkgs, lib, ... }:

with lib;
{
  imports = [
    ../../program/plex/macos-module.nix
    ../../program/plex
    ../../program/calibre-web/macos-module.nix
    ../../program/calibre-web
  ];

  programs.home-manager.enable = true;
  home.homeDirectory = "/Users/server";

  services.calibre-web.options.calibreLibrary = pkgs.lib.mkForce null;
  services.calibre-web.user = "server";

  services.plex.user = "server";

  home.stateVersion = "22.05";
}

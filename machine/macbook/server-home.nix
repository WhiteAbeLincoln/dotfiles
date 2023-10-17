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
  services.calibre-web.group = "staff";

  services.plex.user = "server";
  services.plex.group = "staff";

  home.stateVersion = "22.05";
}

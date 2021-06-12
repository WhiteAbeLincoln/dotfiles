{ config, pkgs, lib, ... }:

with lib;
{
  imports = [
    ../../role/general.nix
    ../../role/dev.nix
    ../../role/devgui.nix
  ];

  programs.home-manager.enable = true;
  home.homeDirectory = "/Users/abe";

  home.activation = {
    copyApplications = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in lib.hm.dag.entryAfter [ "writeBoundary" ] (
      (import ../../lib/funcs.nix).installAppScript apps ''$HOME/Applications/Home Manager Apps''
    );
  };

  home.stateVersion = "21.03";
}

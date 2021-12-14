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

  # vscode doesn't install properly from nix on macos 11
  # and I'm not using nix to sync vscode settings anymore
  programs.vscode.enable = lib.mkForce false;

  # link the home-manager apps. Spotlight and launchpad don't recognize symlinks but Raycast does
  # which means I don't have to go through the trouble of copying or aliasing the apps.
  home.file."Applications/Home Manager Apps".source = let
    apps = pkgs.buildEnv {
      name = "home-manager-applications";
      paths = config.home.packages;
      pathsToLink = "/Applications";
    };
  in "${apps}/Applications";

  home.stateVersion = "21.03";
}

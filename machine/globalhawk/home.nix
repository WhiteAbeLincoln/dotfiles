{ config, pkgs, lib, ... }:

{
  imports = [
    ../../role/dev.nix
  ];

  # home.sessionVariables = {
  #   EDITOR = "vim";
  # };

  home.stateVersion = "23.11";
}

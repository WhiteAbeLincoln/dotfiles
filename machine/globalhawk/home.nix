{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../../role/dev.nix
    ../../program/zellij
  ];

  # home.sessionVariables = {
  #   EDITOR = "vim";
  # };
}

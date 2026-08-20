{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../../program/ai-agents
  ];

  # home.sessionVariables = {
  #   EDITOR = "vim";
  # };
  home.packages = [
    pkgs.unstable.zed-editor
  ];
}

{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../../program/git
    ../../program/vim
    ../../program/fish
    ../../program/starship
    ../../program/direnv
    ../../program/ai-agents
  ];

  # home.sessionVariables = {
  #   EDITOR = "vim";
  # };
  home.packages = [
    pkgs.unstable.zed-editor
  ];
}

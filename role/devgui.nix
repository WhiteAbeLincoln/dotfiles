{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = [
    ../program/vscode
    # ../program/firefox
    # ../program/emacs
    ../program/kitty
  ];

  home.packages = with pkgs; [
    chromium
  ];
}

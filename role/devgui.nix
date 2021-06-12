{ pkgs, lib, ... }:

with lib;

{
  imports = [
    ../program/vscode
    ../program/firefox
    ../program/emacs
    ../program/kitty
    ../program/tmux
  ];

  home.packages = with pkgs; [
    chromium
  ];
}

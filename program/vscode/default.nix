{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
  } // ((import ./settings.nix) pkgs);
}

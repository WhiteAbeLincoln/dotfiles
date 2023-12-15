{ pkgs, ... }:

{
  imports = [
    ./module.nix
  ];
  programs.zsh = {
    enable = true;
    # fig = pkgs.stdenv.isDarwin;
    enableCompletion = false;
  };
}

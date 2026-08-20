{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./module.nix
  ];
  programs.tmux-custom =
    {
      enable = lib.mkDefault true;
    }
    // ((import ./settings.nix) pkgs);
}

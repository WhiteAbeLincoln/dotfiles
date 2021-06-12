{ pkgs, ... }:

{
  imports = [
    ./system-defaults/finder.nix
    ./system-defaults/defaults-writer.nix
  ];
}

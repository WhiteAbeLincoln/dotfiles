args@{ lib, pkgs, ... }:

lib.mkIf pkgs.stdenv.isDarwin (import ./macos-module.nix args)

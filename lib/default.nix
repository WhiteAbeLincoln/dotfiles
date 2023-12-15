args@{ pkgs, lib, ... }:

{
  lib.awhite = {
    types = (import ./types.nix) args;
    mkApplication = (import ./funcs.nix).mkApplication;
    installAppScript = (import ./funcs.nix).installAppScript;
    launchdAgent = (import ./funcs.nix).launchdAgent pkgs;
  };
}

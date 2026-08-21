{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.ghostty;
  package =
    if pkgs.stdenv.hostPlatform.isDarwin
    then pkgs.ghostty-bin
    else pkgs.ghostty;
in {
  options.programs.ghostty.enable = lib.mkEnableOption "the Ghostty graphical terminal emulator";

  config.environment.systemPackages =
    [package.terminfo]
    ++ lib.optional cfg.enable package;
}

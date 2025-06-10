# ENVIRONMENTS: nix-darwin, home-manager
{
  pkgs,
  lib,
  isHM,
  ...
}:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
// (lib.optionalAttrs isHM {
  home.sessionVariables = {
    DIRENV_INSTDIR = "${pkgs.direnv}";
  };
})

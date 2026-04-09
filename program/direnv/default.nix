# ENVIRONMENTS: nix-darwin, home-manager
{
  pkgs,
  pkgs-unstable,
  lib,
  isHM,
  ...
}:
{
  programs.direnv = {
    enable = true;
    package = pkgs-unstable.direnv;
    nix-direnv.enable = true;
  };
}
// (lib.optionalAttrs isHM {
  home.sessionVariables = {
    DIRENV_INSTDIR = "${pkgs-unstable.direnv}";
  };
})

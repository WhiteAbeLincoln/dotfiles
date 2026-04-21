# ENVIRONMENTS: nix-darwin, home-manager
{
  pkgs,
  lib,
  ...
}:
# Loaded at the darwin system level (machine/nighthawk/default.nix) as well
# as in home-manager contexts. `home.sessionVariables` doesn't exist in
# nix-darwin, so gate it on the lib extension home-manager adds.
{
  programs.direnv = {
    enable = true;
    package = pkgs.unstable.direnv;
    nix-direnv.enable = true;
  };
}
// (lib.optionalAttrs (lib ? hm) {
  home.sessionVariables = {
    DIRENV_INSTDIR = "${pkgs.unstable.direnv}";
  };
})

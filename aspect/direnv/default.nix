# ENVIRONMENTS: nix-darwin, home-manager
{lib, ...}: let
  # Loaded through the darwin system projection as well as in home-manager
  # contexts. `home.sessionVariables` is therefore kept in the latter only.
  settings = {pkgs, ...}: {
    programs.direnv = {
      enable = lib.mkDefault true;
      package = pkgs.unstable.direnv;
      nix-direnv.enable = lib.mkDefault true;
    };
  };
in {
  darwin = settings;
  homeManager = {pkgs, ...}: {
    imports = [settings];
    home.sessionVariables.DIRENV_INSTDIR = "${pkgs.unstable.direnv}";
  };
}

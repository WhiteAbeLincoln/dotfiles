# TODO: nix-index works under darwin too
# but should we have it enabled for both
{inputs, lib, ...}: {
  homeManager = {pkgs, ...}: {
    imports = [
      inputs.nix-index-database.hmModules.nix-index
    ];
    programs.nix-index.enable = lib.mkDefault true;
    programs.nix-index-database.comma.enable = lib.mkDefault true;
  };
}

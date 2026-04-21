# HM-layer analogue of modules/common/meta.nix. The unstable overlay
# lives at the flake.nix pkgs-construction site because standalone HM
# doesn't support nixpkgs.overlays as a module option.
{lib, ...}: {
  options.meta = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "Primary user of this host.";
    };
    isWSL = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether this host runs inside WSL.";
    };
  };
}

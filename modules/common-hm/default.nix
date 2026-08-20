# HM-layer analogue of modules/common/meta.nix. Package policy is selected
# through aspect/nixpkgs and applied by the standalone HM constructor in
# modules/aspect/lib.nix because standalone HM doesn't support nixpkgs.overlays
# as a module option.
{lib, ...}: {
  imports = [./defaults.nix];

  options.meta = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "Primary user of this host.";
    };
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "Inventory hostname of this configuration.";
    };
    isWSL = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether this host runs inside WSL.";
    };
  };
}

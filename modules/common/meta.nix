# Host-level metadata exposed as module options, replacing the
# `myUserName`/`isWSL` specialArgs that used to thread through make-cfg.nix.
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

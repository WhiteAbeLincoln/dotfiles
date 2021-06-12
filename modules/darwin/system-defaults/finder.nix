{ config, lib, ... }:

with lib;
{
  options = {
    system.defaults.finder.AppleShowAllFiles = mkOption {
      type = types.nullOr types.bool;
      default = null;
      description = ''
        Whether to always show hidden files. The default is false.
      '';
    };
  };
}

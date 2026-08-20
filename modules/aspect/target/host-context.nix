{lib, ...}: {
  options.dotfiles.host = lib.mkOption {
    readOnly = true;
    type = lib.types.submodule {
      options = {
        class = lib.mkOption {type = lib.types.enum ["nixos" "darwin" "homeManager"];};
        system = lib.mkOption {type = lib.types.str;};
        hostName = lib.mkOption {type = lib.types.str;};
        user = lib.mkOption {type = lib.types.str;};
      };
    };
    description = "Normalized inventory facts for the configuration being evaluated.";
  };
}

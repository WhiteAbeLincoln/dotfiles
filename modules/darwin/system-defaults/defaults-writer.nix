{ config, lib, ... }:

with lib;
let
  cfg = config.system.defaults-writer;
  isFloat = x: isString x && builtins.match "^[+-]?([0-9]*[.])?[0-9]+$" x != null;

  boolValue = x: if x then "YES" else "NO";

  writeValue = value:
    if isBool value then "-bool ${boolValue value}" else
    if isInt value then "-int ${toString value}" else
    if isFloat value then "-float ${toString value}" else
    if isString value then "-string '${value}'" else
    throw "invalid value type";

  writeDefault = domain: key: value:
    "defaults write ${domain} '${key}' ${writeValue value}";

  mapValue = value:
    if isAttrs value then (getAttrs [ "system" "value" ] value) else
      { system = false; value = value; };

  defaultsToList = domain: attrs: mapAttrsToList (writeDefault domain) (filterAttrs (n: v: v != null) attrs);

  defs = let
    cfgList = flatten (mapAttrsToList (domain: mapAttrsToList (key: value: { inherit domain key; value=(mapValue value); })) cfg);
    partitioned = partition (v: v.value.system) cfgList;
    mapDefault = v: writeDefault v.domain v.key v.value.value;
  in
   { system = map mapDefault partitioned.right;
     user = map mapDefault partitioned.wrong;
   };
in
{
  options = {
    system.defaults-writer = mkOption {
      type = types.attrsOf (types.attrsOf types.anything);
      default = {};
      description = ''
        An attribute list of domains containing key value records.
      '';
    };
  };

  config = {
    system.activationScripts = {
      defaults.text = (mkIf (length defs.system != 0)) ''
        # Set extra defaults
        echo >&2 "system defaults continued..."
        ${concatStringsSep "\n" defs.system}
      '';
      userDefaults.text = (mkIf (length defs.user != 0)) ''
        # Set extra defaults
        echo >&2 "user defaults continued..."
        ${concatStringsSep "\n" defs.user}
      '';
    };
  };
}

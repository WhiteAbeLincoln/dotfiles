# from https://github.com/bangedorrunt/nix/blob/f5a8a5d2f023f7d6558b0ce7051ff5e258860f55/lib/default.nix
# SEE https://github.com/NixOS/nixpkgs/blob/master/lib/fixed-points.nix
{lib, ...} @ args:
with lib; let
  _lib = self: let
    callLibs = file: import file ({lib = self;} // args);
  in {
    types = callLibs ./types.nix;
    darwin = callLibs ./darwin.nix;
    # NOTE these are magically handled by `mine.extend` implementation below.
    # inherit (self.attrs) mergeAny;
    # inherit (self.importers) rakeLeaves flattenTree;
    # inherit (self.options) mkEnableOpt' mkOpt mkOpt' mkOptStr mkBoolOpt;
  };
  # NOTE `makeExtensible` allows `self` referencing
  mine = makeExtensible _lib;
in
  mine
  # .extend (self: super:
  #   foldr (a: b: a // b) {} (attrValues super))

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.git;
  trim = let
    _trim = p: r: s: if p " " s || p "\t" s then
      _trim p r (r " " (r "\t" s))
      else s;
    trimLeft = _trim hasPrefix removePrefix;
    trimRight = _trim hasSuffix removeSuffix;
  in
    s: trimLeft (trimRight s);
in

{
  options = {
    programs.git = {
      ignoreFiles = mkOption {
        default = [];
        description = ''
          Additional ignore files
        '';
      };
    };
  };
  config = mkIf (cfg.ignoreFiles != []) {
    programs.git.ignores = concatMap (p: filter (s: (trim s) != "") (splitString "\n" (fileContents p))) cfg.ignoreFiles;
  };
}

{ config, lib, pkgs, isWSL, modulesPath, ... }:

let
  cfg = config.windows.file;
  fileType = (import ${modulesPath}/lib/file-type.nix {
    inherit
  }).fileType;
in
{
  options.windows.file = {

  };
}
lib.mkIf isWSL (

)

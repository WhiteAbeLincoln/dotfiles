{ config, lib, pkgs, modulesPath, ... }:

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
lib.mkIf config.meta.isWSL (

)

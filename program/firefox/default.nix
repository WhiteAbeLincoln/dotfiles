{ pkgs, ... } @ args:

{
  imports = [
    ./module.nix
  ];
  programs.firefox = {
    enable = true;
    extraPackageConfig = {
      enableTridactylNative = true;
    };
  } // ((import ./settings.nix) args);
}

{ pkgs, lib, ... }:

{
  # importing this conditionally causes recursion issues
  # instead, macos machines will be required to import the module statically
  # imports = [./macos-module.nix];

  services.calibre-web = {
    enable = true;
    listen.ip = "0.0.0.0";
    options = {
      enableBookUploading = true;
      calibreLibrary = /data/Media/books;
    };
    user = lib.mkIf pkgs.stdenv.isDarwin "abe";
    group = lib.mkIf pkgs.stdenv.isDarwin "staff";
  };
}

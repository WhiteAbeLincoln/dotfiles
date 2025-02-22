{ pkgs, ... }:

{
  # importing this conditionally causes recursion issues
  # instead, macos machines will be required to import the module statically
  # imports = [./macos-module.nix];

  services.calibre-web = {
    enable = true;
    listen.ip = "0.0.0.0";
    options = {
      enableBookUploading = true;
      calibreLibrary = "/data/Media/books";
    };
  };
}

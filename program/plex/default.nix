{ pkgs, ... }:

{
  # importing this conditionally causes recursion issues
  # instead, macos machines will be required to import the module statically
  # imports = [./macos-module.nix];

  services.plex = {
    enable = true;
    openFirewall = true;
    extraScanners = [
      (pkgs.fetchFromGitHub {
        owner = "ZeroQI";
        repo = "Absolute-Series-Scanner";
        rev = "b33b1935480cae76007a82f8887cb173200cfc53";
        sha256 = "YupSXgFi/qfODuay3LoIl/1178gtU+MwhiZJAvvGV2g=";
      })
    ];
    extraPlugins = [
      (builtins.path {
        name = "Hama.bundle";
        path = pkgs.fetchFromGitHub {
          owner = "ZeroQI";
          repo = "Hama.bundle";
          rev = "c6987a00e68b23883a263481c823bb7aa7684c21";
          sha256 = "pH7oO0dsTA2zXsquwCV6z8IdNoDwippP806KT9TX4RU=";
        };
      })
      # an audiobook library organizer
      # (builtins.path {
      #   name = "Audnexus.bundle";
      #   path = pkgs.fetchFromGitHub {
      #     owner = "djdembeck";
      #     repo = "Audnexus.bundle";
      #     rev = "v0.2.8";
      #     sha256 = "sha256-IWOSz3vYL7zhdHan468xNc6C/eQ2C2BukQlaJNLXh7E=";
      #   };
      # })
    ];
  };
}

{ pkgs, ... }:

{
  imports = [
    ../program/firefox
    ../program/zathura
    ../program/tridactyl
  ];

  home.packages = with pkgs; [
    chromium
    spotify
    # zoom-us
    # libreoffice-qt
    # hunspell
    # tightvnc
  ];
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };
  programs.command-not-found.enable = true;
  programs.htop = {
    enable = true;
    settings = {
      vim_mode = true;
    };
  };
}

{ pkgs, ... }:

{
  imports = [
    ../program/firefox
    ../program/zathura
    ../program/tridactyl
  ];

  home.packages = with pkgs; [
    google-chrome
    spotify
    zoom-us
    libreoffice-qt
    hunspell
    tightvnc
  ];
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };
  programs.feh.enable = true;
}

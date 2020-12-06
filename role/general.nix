{ pkgs, ... }:

{
  imports = [
    ../program/firefox
    ../program/zathura
  ];

  home.packages = with pkgs; [
    google-chrome
    spotify
    zoom-us
    libreoffice-qt
    hunspell
  ];
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };
  programs.feh.enable = true;
}

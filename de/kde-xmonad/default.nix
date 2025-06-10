{pkgs, ...}: {
  imports = [
    ../../program/rofi
    ../../program/abes-xmonad
  ];

  home.packages = [
    pkgs.kdeApplications.ark
    pkgs.lxqt.pavucontrol-qt
  ];

  programs.abes-xmonad.enableKde = true;
  programs.firefox.extraPackageConfig.enablePlasmaBrowserIntegration = true;
}

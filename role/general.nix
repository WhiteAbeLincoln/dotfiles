{pkgs, ...}: {
  imports = [
    # ../program/firefox
    ../program/zathura
    # ../program/tridactyl
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
    # not using LaTeX right now
    enable = false;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };
  # adds command-not-found handler
  programs.nix-index.enable = true;
  programs.htop = {
    enable = true;
    settings = {
      vim_mode = true;
    };
  };
}

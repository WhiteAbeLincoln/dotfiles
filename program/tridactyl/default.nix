{pkgs, ...} @ args: {
  imports = [
    ./module.nix
  ];
  programs.tridactyl = {
    enable = true;
    enableNative = true;
    tridactylrc.source = ./tridactylrc;
  };
}

{pkgs, ...} @ args: {
  imports = [
    ./module.nix
  ];
  programs.firefox =
    {
      enable = true;
    }
    // ((import ./settings.nix) args);
}

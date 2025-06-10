{pkgs, ...}: {
  imports = [
    ./module.nix
  ];
  programs.git =
    {
      enable = true;
    }
    // (import ./settings.nix);
}

{pkgs, ...}: {
  imports = [
    ./module.nix
  ];
  programs.git =
    {
      enable = true;
    }
    // (import ./settings.nix);
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      number = true;
      syntax-theme = "Github";
    };
  };
}

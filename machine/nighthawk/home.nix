{
  pkgs,
  pkgs-unstable,
  ...
}: let
  secrets = import ../../secrets/common.nix;
in {
  imports = [
    ../../program/git
    ../../program/vim
    ../../program/fish
    ../../program/starship
    ../../program/wezterm
  ];

  home.packages = [
    pkgs.haskellPackages.ShellCheck
    pkgs.podman
  ];

  # programs.texlive = {
  #   enable = true;
  #   extraPackages = tpkgs: {
  #     inherit (tpkgs) scheme-full;
  #   };
  # };
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.extraConfig = ''
    IgnoreUnknown AddKeysToAgent,UseKeychain
    AddKeysToAgent yes
    UseKeychain yes
  '';

  programs.nix-index.enable = true;
  programs.fish.shellAliases = {
    docker = "podman";
  };

  programs.rbw = {
    enable = true;
    package = pkgs.rbw;
    settings = {
      email = secrets.bw_email;
      pinentry = pkgs.pinentry_mac;
    };
  };

  home.stateVersion = "24.05";
}

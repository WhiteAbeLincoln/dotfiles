{pkgs, ...}: {
  imports = [
    ../../program/git
    ../../program/vim
    ../../program/fish
    ../../program/starship
    ../../program/wezterm
    ../../program/emacs
  ];

  home.packages = [
    pkgs.haskellPackages.ShellCheck
  ];

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.extraConfig = ''
    IgnoreUnknown AddKeysToAgent,UseKeychain
    AddKeysToAgent yes
    UseKeychain yes
  '';
  programs.keychain.enable = pkgs.stdenv.isLinux;

  programs.nix-index.enable = true;

  home.stateVersion = "24.05";
}

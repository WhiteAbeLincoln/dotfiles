{
  pkgs,
  inputs,
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
    pkgs.diff2html-cli
    pkgs.haskellPackages.ShellCheck
    pkgs.podman
    pkgs.docker-compose
    pkgs.difftastic
    inputs.git-different.packages.${pkgs.system}.default
    # cat replacement
    pkgs.bat
    # find alternative (not command line compatible)
    pkgs.fd
    pkgs.tmux
    pkgs.imagemagick
    pkgs.ripgrep
    # ls replacement https://github.com/eza-community/eza
    pkgs.eza
    # a system monitor, alternative to top https://github.com/ClementTsang/bottom
    pkgs.bottom
    # a modern alternative to curl https://github.com/ducaale/xh
    pkgs.xh
    pkgs.lazygit
    # OpenAI codex cli
    pkgs.unstable.codex
  ];

  # programs.texlive = {
  #   enable = true;
  #   extraPackages = tpkgs: {
  #     inherit (tpkgs) scheme-full;
  #   };
  # };
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.enableDefaultConfig = false;
  programs.ssh.matchBlocks."*" = {
    extraOptions = {
      IgnoreUnknown = "AddKeysToAgent,UseKeychain";
      AddKeysToAgent = "yes";
      UseKeychain = "yes";
    };
  };

  programs.nix-index.enable = true;
  programs.fish.shellAliases = {
    # docker = "podman";
    cat = "bat --paging=never";
    ll = "eza -F -l --git --hyperlink";
    # start with depth 2 by default, luckily eza allows overriding
    # the level flag by providing it again, so I can tack on another
    # when using the alias to go deeper.
    # in many directories it runs into max filedescriptor limits
    # if we run without a depth limit so 2 is a reasonable default.
    # I can always override with a big depth if it matters.
    tree = "eza -F -l --git --hyperlink --tree --level=2";
    ls = "eza -F --hyperlink";
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

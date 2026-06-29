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
    ../../program/ai-agents
    ../../program/userscripts
  ];

  home.packages = [
    pkgs.nil
    pkgs.diff2html-cli
    pkgs.haskellPackages.ShellCheck
    # Lima-based Docker daemon for macOS — works with Tilt + kind where
    # podman 5's Docker API compat layer falls down (BuildKit gRPC, kind
    # load docker-image both fail on podman). Docker CLI is a separate
    # package since pkgs.colima only provides the daemon manager.
    pkgs.unstable.colima
    pkgs.unstable.docker-client
    pkgs.difftastic
    inputs.git-different.packages.${pkgs.system}.default
    # cat replacement
    pkgs.bat
    # find alternative (not command line compatible)
    pkgs.fd
    pkgs.tmux
    pkgs.zellij
    pkgs.imagemagick
    pkgs.ripgrep
    # ls replacement https://github.com/eza-community/eza
    pkgs.eza
    # a system monitor, alternative to top https://github.com/ClementTsang/bottom
    pkgs.bottom
    # a modern alternative to curl https://github.com/ducaale/xh
    pkgs.xh
    pkgs.lazygit
    # Work stuff
    pkgs.glab
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
  # Default host block. As of home-manager 26.05, matchBlocks/extraOptions are
  # deprecated in favour of `settings` keyed by host, with OpenSSH directive
  # names used directly. IgnoreUnknown keeps older ssh from choking on the
  # macOS-only UseKeychain directive.
  programs.ssh.settings."*" = {
    IgnoreUnknown = "AddKeysToAgent,UseKeychain";
    AddKeysToAgent = "yes";
    UseKeychain = "yes";
  };

  programs.nix-index.enable = true;
  programs.fish.shellAliases = {
    # docker = "podman";
    cat = "bat --paging=never";
    ll = "eza --classify --long --all --header --git --hyperlink";
    # start with depth 2 by default, luckily eza allows overriding
    # the level flag by providing it again, so I can tack on another
    # when using the alias to go deeper.
    # in many directories it runs into max filedescriptor limits
    # if we run without a depth limit so 2 is a reasonable default.
    # I can always override with a big depth if it matters.
    tree = "eza --classify --long --git --hyperlink --tree --level=2";
    ls = "eza --classify --hyperlink";
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

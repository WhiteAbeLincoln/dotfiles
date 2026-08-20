{
  pkgs,
  inputs,
  ...
}: let
  secrets = import ../../secrets/common.nix;
in {
  imports = [
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
    pkgs.unstable.ollama
    pkgs.unstable.lmstudio
    pkgs.difftastic
    inputs.git-different.packages.${pkgs.system}.default
    pkgs.tmux
    pkgs.zellij
    pkgs.imagemagick
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

  programs.rbw = {
    enable = true;
    package = pkgs.rbw;
    settings = {
      email = secrets.bw_email;
      pinentry = pkgs.pinentry_mac;
    };
  };
}

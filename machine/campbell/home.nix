{
  config,
  pkgs,
  ...
}: let
  secrets = import ../../secrets/common.nix;
in {
  imports = [
    ../../program/git
    ../../program/vim
    ../../program/fish
    ../../program/direnv
    ../../program/starship
    # ../../program/emacs
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.

  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  home.sessionVariables = {
    AWS_PROFILE = "m2i";
  };

  home.packages = [
    pkgs.nil
    pkgs.xclip
    pkgs.hostname
    pkgs.git
    pkgs.openssh_gssapi
    pkgs.amazon-ecr-credential-helper
    pkgs.bitwarden-cli
    pkgs.git-crypt
  ];

  programs.rbw = {
    enable = true;
    package = pkgs.rbw;
    settings = {
      email = secrets.bw_email;
      pinentry = pkgs.pinentry-curses;
    };
  };

  programs.ssh.enable = true;
  programs.keychain.enable = pkgs.stdenv.isLinux;
  programs.keychain.keys = ["id_ed25519"];

  programs.git = {
    # rhel based distros have configs expecting a patched openssh which supports gssapi
    # the gitFull package uses a nix openssh build instead of the global one, so we must
    # override with the patched version https://github.com/NixOS/nixpkgs/issues/160527
    package = pkgs.git.override {openssh = pkgs.openssh_gssapi;};
    userEmail = pkgs.lib.mkForce "awhite@campbellsci.com";
    ignoreFiles = [
      # we don't want to check in nix things for campbell projects
      ../../program/git/ignores/nixshell.ignore
      ../../program/git/ignores/visualstudio.ignore
    ];
    # extraConfig = {
    #   url = {
    #     "ssh://git@gitlab.com" = {
    #       insteadOf = "https://gitlab.com/";
    #     };
    #   };
    # };
  };
  # programs.texlive = {
  # enable = true;
  # extraPackages = tpkgs: {
  # inherit (tpkgs) scheme-full;
  # };
  # };
  virtualisation.docker.rootless.enable = true;

  windows.environment = {
    enable = false;
    variables = rec {
      BASH_ENV = "~/.bash_env_noninteractive";
      KOMOREBI_CONFIG_HOME = "%APPDATA%\\komorebi";
      WHKD_CONFIG_HOME = KOMOREBI_CONFIG_HOME;
    };
    wslenv = {
      BASH_ENV = {for-wsl = true;};
    };
  };
}

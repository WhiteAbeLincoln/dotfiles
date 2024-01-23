{ config, pkgs, ... }:

{
  imports = [
    ../../packages/nur
    ../../role/dev.nix
    ../../program/emacs
    ../../modules/windows/winget
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = [
    pkgs.rnix-lsp
  ];
  programs.texlive.enable = true;
  programs.texlive.extraPackages = tpkgs: { inherit (tpkgs) scheme-full; };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "awhite";
  home.homeDirectory = "/home/awhite";
  home.file.WinHome.source = config.lib.file.mkOutOfStoreSymlink "/mnt/c/Users/awhite";
  # ensures our wsl-git will work
  # make sure you've set the windows environment variables
  # BASH_ENV: ~/.bash_env_noninteractive
  # WSLENV: BASH_ENV/u
  home.file.".bash_env_noninteractive".text = ''
  if [ -e /home/awhite/.nix-profile/etc/profile.d/nix.sh ]; then
    . /home/awhite/.nix-profile/etc/profile.d/nix.sh
  fi
  '';

  programs.git = {
    # use gitFull so that we get git-svn
    # rhel based distros have configs expecting a patched openssh which supports gssapi
    # the gitFull package uses a nix openssh build instead of the global one, so we must
    # override with the patched version https://github.com/NixOS/nixpkgs/issues/160527
    package = pkgs.gitFull.override { openssh = pkgs.openssh_gssapi; };
    userEmail = pkgs.lib.mkForce "awhite@campbellsci.com";
    extraConfig.svn.rmdir = true;
    ignoreFiles = [
      # we don't want to check in nix things for campbell projects
      ../../program/git/ignores/nixshell.ignore
      ../../program/git/ignores/visualstudio.ignore
    ];
    svnHooks = true;
  };

  winget.enable = true;
  winget.sources = {
    winget = {
      Type = "Microsoft.PreIndexed.Package";
      Argument = "https://winget.azureedge.net/cache";
      Identifier = "Microsoft.Winget.Source_8wekyb3d8bbwe";
    };
  };
  winget.packages = [
    "Microsoft.VisualStudioCode"
    {
      PackageIdentifier = "Microsoft.Powertoys";
      Version = "0.15.2";
      Source = "msstore";
    }
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}

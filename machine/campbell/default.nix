{ config, pkgs, ... }:

{
  imports = [
    ../../packages/nur
    ../../role/dev.nix
    ../../modules/windows/winget
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "awhite";
  home.homeDirectory = "/home/awhite";

  programs.git = {
    # use gitFull so that we get git-svn
    package = pkgs.gitFull;
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

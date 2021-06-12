{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nix-darwin>
    ../../modules/darwin
    ../../packages/nur
    ../../program/yabai
    ../../program/skhd
  ];

  system = {
    defaults-writer = {
      "com.apple.dock" = {
        show-recents = false;
      };
      "com.apple.finder" = {
        ShowPathbar = true;
      };
      "com.apple.menuextra.clock" = {
        DateFormat = "d MMM HH:mm";
      };
    };
    defaults = {
      dock.mru-spaces = false; # don't rearrange spaces by most recent use, needed for yabai
      dock.autohide = true;
      finder = {
        AppleShowAllFiles = true;
        AppleShowAllExtensions = true;
        CreateDesktop = false; # hide desktop icons
        _FXShowPosixPathInTitle = true;
      };
      trackpad = {
        Clicking = true;
      };
      loginwindow = {
        GuestEnabled = false;
      };
      NSGlobalDomain = {
        AppleFontSmoothing = 0;
        ApplePressAndHoldEnabled = false; # disable accent popup on keypress
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSDocumentSaveNewDocumentsToCloud = false;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };

    activationScripts.applications.text = pkgs.lib.mkForce ((import ../../lib/funcs.nix).installAppScript config.system.build.applications ''/Applications/Nix Apps'');
  };

  nixpkgs.config = {
    allowUnfree = true;
  };
  nixpkgs.overlays = (import ../../packages/overlays/darwin.nix);

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  users.users.abe = {
    description = "Abraham White";
    home = "/Users/abe";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.users.abe = import ./home.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}

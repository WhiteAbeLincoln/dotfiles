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

    activationScripts.applications.text = let
      apps = config.system.build.applications;
    in ''
      echo "setting up ~/Applications/Nix Apps..." >&2

      if [ ! -e ~/Applications/Nix\ Apps -o -L ~/Applications/Nix\ Apps ]; then
        ln -sfn ${apps}/Applications ~/Applications/Nix\ Apps
      else
        echo "warning: ~/Applications and ~/Applications/Nix Apps are directories, skipping App linking..." >&2
      fi
    '';
  };

  nixpkgs.config = {
    allowUnfree = true;
  };
  nixpkgs.overlays = (import ../../packages/overlays/darwin.nix) ++ [
    # I disable installing kitty package because there were errors with the bs4 dependency
    (self: super: { kitty = pkgs.runCommandLocal "no-kitty" {} "mkdir $out"; })
    # yabai is having some issues building from source on macos 11 with nix
    (self: super: {
      yabai-binary = super.yabai.overrideAttrs (
        o: rec {
          version = "3.3.10";
          src = builtins.fetchTarball {
            url = "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
            sha256 = "025ww9kjpy72in3mbn23pwzf3fvw0r11ijn1h5pjqvsdlak91h9i";
          };

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/man/man1/
            cp ./archive/bin/yabai $out/bin/yabai
            cp ./archive/doc/yabai.1 $out/share/man/man1/yabai.1
          '';
        }
      );
    })
  ];

  environment.systemPackages =
    [ pkgs.vim ];

  services.yabai-custom.package = pkgs.yabai-binary;
  services.yabai-custom.bigSurScriptingAddition = true;

  # services.nix-daemon.enable = true;
  # users.nix.configureBuildUsers = true;

  programs.zsh.enable = true;

  users.users.abe = {
    description = "Abraham White";
    home = "/Users/abe";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.users.abe = import ./home.nix;

  system.stateVersion = 4;
}

{
  config,
  pkgs,
  ...
}: {
  system = {
    defaults-writer = {
      "com.apple.dock" = {
        show-recents = false;
      };
      "com.apple.finder" = {
        ShowPathbar = true;
        AppleShowAllFiles = true;
      };
      "com.apple.menuextra.clock" = {
        DateFormat = "d MMM HH:mm";
      };
      "com.apple.loginwindow".SHOWOTHERUSERS_MANAGED = {
        system = true;
        value = false;
      };
    };
    defaults = {
      dock.mru-spaces = false; # don't rearrange spaces by most recent use, needed for yabai
      dock.autohide = true;
      finder = {
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

    # activationScripts.applications.text = let
    #   apps = config.system.build.applications;
    #   home = config.users.users.abe.home;
    # in ''
    #   echo "setting up ~/Applications/Nix Apps..." >&2

    #   if [ ! -e ${home}/Applications/Nix\ Apps -o -L ${home}/Applications/Nix\ Apps ]; then
    #     ln -sfn ${apps}/Applications ${home}/Applications/Nix\ Apps
    #   else
    #     echo "warning: ${home}/Applications and ${home}/Applications/Nix Apps are directories, skipping App linking..." >&2
    #   fi
    # '';
  };

  # programs.zsh.shellInit = ''
  # if [ -x /usr/libexec/path_helper ]; then
  #   function () {
  #     local OLDP="$PATH"
  #     eval "$(/usr/libexec/path_helper -s)"
  #     export PATH="$OLDP:$PATH"
  #     typeset -U path
  #   }
  # fi
  # export OLD_NIX_PATH="$NIX_PATH";
  # '';
  # programs.zsh.interactiveShellInit = ''
  # if [ -n "$OLD_NIX_PATH" ]; then
  #   if [ "$OLD_NIX_PATH" != "$NIX_PATH" ]; then
  #     NIX_PATH="$OLD_NIX_PATH"
  #     echo Reset NIX_PATH
  #   fi
  #   unset OLD_NIX_PATH
  # fi
  # '';

  services.nix-daemon.enable = true;
  nix.configureBuildUsers = true;

  programs.zsh.enable = true;

  fonts.packages = [pkgs.cascadia-code];

  security.pam.enableSudoTouchIdAuth = true;
}

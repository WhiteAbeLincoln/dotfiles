# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;

  programs.firefox.enable = true;
  programs.steam = {
    enable = true;
    gamescopeSession = {
      enable = true;
      args = [
        "--prefer-vk-device"
        "1002:687f"
        "--prefer-output"
        "HDMI-A-2"
      ];
    };
    package = pkgs.steam.override {
      extraEnv.DRI_PRIME = "pci-0000_03_00_0";
    };
    protontricks.enable = true;
  };
  programs.gamemode.enable = true;
  programs.gamescope = {
    capSysNice = true;
    package = pkgs.gamescope.overrideAttrs (old: {
      patches =
        (old.patches or [])
        ++ [
          (pkgs.writeText "gamescope-drop-cap-sys-nice.patch" ''
            diff --git a/src/Utils/Process.cpp b/src/Utils/Process.cpp
            --- a/src/Utils/Process.cpp
            +++ b/src/Utils/Process.cpp
            @@ -227,6 +227,10 @@ namespace gamescope::Process
                 static void ProcessPreSpawn()
                 {
                     ResetSignals();
            +#if defined(__linux__) && HAVE_LIBCAP
            +        prctl( PR_CAP_AMBIENT, PR_CAP_AMBIENT_LOWER, CAP_SYS_NICE, 0, 0 );
            +#endif
            +

                     RestoreFdLimit();
                     RestoreNice();
          '')
        ];
    });
  };

  services.xserver.xkb = {
    layout = "us";
    options = "caps:swapescape";
  };
  console.useXkbConfig = true;

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;
  users.users.${config.dotfiles.host.user} = {
    isNormalUser = true;
    extraGroups = [
      "gamemode"
      "wheel"
    ]; # Enable ‘sudo’ for the user.
  };
  services.openssh.enable = true;
}

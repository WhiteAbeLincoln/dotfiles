{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nix-darwin>
    ../../modules/darwin
    ../../role/darwin.nix
    ../../packages/nur
    ../../program/yabai
    ../../program/skhd
  ];

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
          version = "4.0.0";
          src = builtins.fetchTarball {
            url = "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
            sha256 = "1iwzan3mgayfkx7qbbij53hkxvr419b6kmypp7zmvph270yzy4r9";
          };

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/man/man1/
            cp ./bin/yabai $out/bin/yabai
            cp ./doc/yabai.1 $out/share/man/man1/yabai.1
          '';
        }
      );
    })
  ];

  services.yabai-custom.package = pkgs.yabai-binary;
  services.yabai-custom.bigSurScriptingAddition = true;

  homebrew.enable = true;
  homebrew.casks = [ "fig" "kitty" ];
  homebrew.brews = [ "bitwarden-cli" ];

  users.users.abe = {
    description = "Abraham White";
    home = "/Users/abe";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.users.abe = import ./home.nix;

  system.stateVersion = 4;
}

{...}: let
  secrets = import ../secrets/common.nix;
in {
  homeManager = {pkgs, ...}: {
    programs.rbw = {
      enable = true;
      package = pkgs.rbw;
      settings = {
        email = secrets.bw_email;
        pinentry =
          if pkgs.stdenv.isDarwin
          then pkgs.pinentry_mac
          else pkgs.pinentry-curses;
      };
    };
  };
}

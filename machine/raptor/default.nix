{ pkgs, ... }:

{
  home.packages = with pkgs; [
    libinput-gestures # raptor has touchpad
    xdotool # we need this for commands in libinput-gestures
    wmctrl
  ];

  xdg.configFile."libinput-gestures.conf".source = ./libinput-gestures.conf;
}

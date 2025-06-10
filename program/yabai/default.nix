{pkgs, ...}: {
  imports = [
    ./module.nix
  ];

  services.yabai-custom = {
    enable = true;
    config = {
      focus_follows_mouse = "autofocus";
      mouse_follows_focus = "off";
      layout = "bsp";
      window_shadow = "float";
      window_border = "on";
      window_opacity = "on";
      normal_window_opacity = "0.9";
      mouse_modifier = "alt";
      window_topmost = "on";
      window_border_width = 6;
      active_window_border_color = "0xFFADBF8A";
      normal_window_border_color = "0xFF555555";
    };
    extraConfig = ''
      # rules
      yabai -m rule --add app='System Preferences' manage=off
    '';
  };
}

{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    settings = {
      focus_follows_mouse = true;
      active_border_color = "#adbf8a";
      inactive_border_color = "#555555";
      hide_window_decorations = if pkgs.stdenv.isDarwin then "titlebar-only" else true;
      tab_bar_style = "powerline";
      close_on_child_death = true;
      clipboard_control = "write-clipboard write-primary read-clipboard read-primary";
      macos_quit_when_last_window_closed = pkgs.stdenv.isDarwin;
      enabled_layouts = "splits,fat,tall,vertical,horizontal,grid,stack";
      kitty_mod = if pkgs.stdenv.isDarwin then "super+shift" else "ctrl+shift";
      window_padding_width = 4;
    };
    keybindings = {
      "ctrl+f>\\" = "launch --location=vsplit --cwd=current";
      "ctrl+f>-" = "launch --location=hsplit --cwd=current";
      "ctrl+f>x" = "close_window";
      "ctrl+f>c" = "launch --cwd=current";
      "ctrl+f>z" = "kitten zoom_toggle.py";
      "ctrl+f>shift+z" = "last_used_layout";
      "ctrl+f>h" = "neighboring_window left";
      "ctrl+f>j" = "neighboring_window down";
      "ctrl+f>k" = "neighboring_window up";
      "ctrl+f>l" = "neighboring_window right";
      "ctrl+f>r" = "layout_action rotate";
      "kitty_mod+space" = "next_layout";
    };
    extraConfig = ''# Fig Kitty Integration: Enabled
watcher ''${HOME}/.fig/tools/kitty-integration
# End of Fig Kitty Integration'';
  };
  xdg.configFile."kitty/zoom_toggle.py".source = ./kittens/zoom_toggle.py;
}

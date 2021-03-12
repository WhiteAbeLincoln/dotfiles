{ pkgs, ... }:

{
  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
    alt - j : yabai -m window --focus next || yabai -m window --focus first # focus next
    alt - k : yabai -m window --focus prev || yabai -m window --focus last # focus previous

    alt - return : yabai -m window --swap recent # swap window with focused
    alt + shift - j : yabai -m window --swap next || yabai -m window --swap first # swap with next
    alt + shift - k : yabai -m window --swap prev || yabai -m window --swap last # swap with previous
    alt + shift - c : yabai -m window --close # close window

    alt - 1 : yabai -m space --focus 1 # move to workspace 1
    alt - 2 : yabai -m space --focus 2 # move to workspace 2
    alt - 3 : yabai -m space --focus 3 # move to workspace 3
    alt - 4 : yabai -m space --focus 4 # move to workspace 4
    alt - 5 : yabai -m space --focus 5 # move to workspace 5

    alt - r : yabai -m display --focus next # move to next monitor
    alt - e : yabai -m display --focus prev # move to previous monitor

    alt + shift - 1 : yabai -m window --space 1 # shift window to workspace 1
    alt + shift - 2 : yabai -m window --space 2 # shift window to workspace 2
    alt + shift - 3 : yabai -m window --space 3 # shift window to workspace 3
    alt + shift - 4 : yabai -m window --space 4 # shift window to workspace 4
    alt + shift - 5 : yabai -m window --space 5 # shift window to workspace 5

    alt + shift - r : yabai -m window --display next # shift window to next monitor
    alt + shift - e : yabai -m window --display prev # shift window to previous monitor

    :: toggle @

    toggle < escape ; default
    alt + shift - t ; toggle

    toggle < z : yabai -m window --toggle zoom-fullscreen; ${pkgs.skhd}/bin/skhd -k "escape"
    '';
  };
  launchd.user.agents.skhd.serviceConfig.StandardOutPath = "/tmp/skhd.out.log";
  launchd.user.agents.skhd.serviceConfig.StandardErrorPath = "/tmp/skhd.err.log";
}

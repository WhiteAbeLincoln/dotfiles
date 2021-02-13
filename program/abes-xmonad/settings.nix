pkgs: {
  options = {
    dmenu = "rofi";
    dmenuArgs = ["-dmenu" "-i"];
    normalBorderColor = "#555555";
    focusedBorderColor = "#adbf8a";
    borderWidth = 2;
    terminal = {
      keyMap = "M-S-<Return>";
      name = "Launch terminal";
      value = "${pkgs.termite}/bin/termite -e ${pkgs.tmux}/bin/tmux";
    };
    keybindings = {
      "M-p" = { name = "Launch application"; value = "rofi -show drun"; };
      "M-S-p" = { name = "Execute program"; value = "rofi -show run"; };
      "M-<Tab>" = { name = "Window switcher"; value = "rofi -show window"; };
      "M-S-q" = { name = "Logout"; value = "qdbus org.kde.ksmserver /KSMServer logout 1 3 3"; };
    };
  };
}

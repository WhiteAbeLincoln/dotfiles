{lib, ...}: let
  gruvbox = import ../colors.nix;
  palette = gruvbox.palette;
  colors = gruvbox.colors.hard.dark;
in {
  home-theme = {
    rofi.theme = "gruvbox-dark-hard";
    zathura = import ../zathura.nix {
      bg = colors.bg0;
      fg = colors.fg1;
      inherit (colors) bg1 bg2 gray yellow green blue orange red;
    };
    termite = import ../termite.nix {
      bg = colors.bg0;
      fg = colors.fg1;
      color0 = palette.dark0;
      gray = palette.gray_245;

      reds.primary = palette.neutral.red;
      reds.secondary = colors.red;

      greens.primary = palette.neutral.green;
      greens.secondary = colors.green;

      yellows.primary = palette.neutral.yellow;
      yellows.secondary = colors.yellow;

      blues.primary = palette.neutral.blue;
      blues.secondary = colors.blue;

      purples.primary = palette.neutral.purple;
      purples.secondary = colors.purple;

      aquas.primary = palette.neutral.aqua;
      aquas.secondary = colors.aqua;

      # light4 + light1
      contrasts.primary = palette.light4;
      contrasts.secondary = palette.light1;
    };
    vscode = import ../vscode.nix "Gruvbox Dark Hard";
    vim = import ../vim.nix "dark" "hard";
    firefox.default =
      import ../firefox.nix {
        color-scheme = "dark";
        density = "compact";
      }
      colors;
  };
  programs.git.delta.options.dark = true;
}

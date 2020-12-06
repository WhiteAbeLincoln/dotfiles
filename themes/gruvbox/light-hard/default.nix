{lib, ...}:

let
  gruvbox = import ../colors.nix;
  palette = gruvbox.palette;
  colors = gruvbox.colors.hard.light;
in
  {
    home-theme = {
      rofi.theme = "gruvbox-light-hard";
      zathura = import ../zathura.nix {
        bg = colors.bg0;
        fg = colors.fg1;
        inherit (colors) bg1 bg2 gray yellow green blue orange red;
      };
      termite = import ../termite.nix {
        bg = colors.bg0;
        fg = colors.fg1;
        color0 = palette.light0;
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

        contrasts.primary = palette.dark4;
        contrasts.secondary = palette.dark1;
      };
      vscode = import ../vscode.nix "Gruvbox Light Hard";
      vim = import ../vim.nix "light" "hard";
      firefox.default = import ../firefox.nix {color-scheme = "light"; density = "compact";} colors;
    };
    programs.git.delta.options.light = true;
  }

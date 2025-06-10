let
  palette = {
    hard.dark0 = "#1d2021"; # 29-32-33
    dark0 = "#282828"; # 40-40-40
    soft.dark0 = "#32302f"; # 50-48-47
    dark1 = "#3c3836"; # 60-56-54
    dark2 = "#504945"; # 80-73-69
    dark3 = "#665c54"; # 102-92-84
    dark4 = "#7c6f64"; # 124-111-100
    dark4_256 = "#7c6f64"; # 124-111-100

    gray_245 = "#928374"; # 146-131-116
    gray_244 = "#928374"; # 146-131-116
    gray_246 = "#a89984";

    hard.light0 = "#f9f5d7"; # 249-245-215
    light0 = "#fbf1c7"; # 253-244-193
    soft.light0 = "#f2e5bc"; # 242-229-188
    light1 = "#ebdbb2"; # 235-219-178
    light2 = "#d5c4a1"; # 213-196-161
    light3 = "#bdae93"; # 189-174-147
    light4 = "#a89984"; # 168-153-132
    light4_256 = "#a89984"; # 168-153-132

    bright.red = "#fb4934"; # 251-73-52
    bright.green = "#b8bb26"; # 184-187-38
    bright.yellow = "#fabd2f"; # 250-189-47
    bright.blue = "#83a598"; # 131-165-152
    bright.purple = "#d3869b"; # 211-134-155
    bright.aqua = "#8ec07c"; # 142-192-124
    bright.orange = "#fe8019"; # 254-128-25

    neutral.red = "#cc241d"; # 204-36-29
    neutral.green = "#98971a"; # 152-151-26
    neutral.yellow = "#d79921"; # 215-153-33
    neutral.blue = "#458588"; # 69-133-136
    neutral.purple = "#b16286"; # 177-98-134
    neutral.aqua = "#689d6a"; # 104-157-106
    neutral.orange = "#d65d0e"; # 214-93-14

    faded.red = "#9d0006"; # 157-0-6
    faded.green = "#79740e"; # 121-116-14
    faded.yellow = "#b57614"; # 181-118-20
    faded.blue = "#076678"; # 7-102-120
    faded.purple = "#8f3f71"; # 143-63-113
    faded.aqua = "#427b58"; # 66-123-88
    faded.orange = "#af3a03";
  };
  colors = rec {
    dark = {
      bg0 = palette.dark0;
      bg1 = palette.dark1;
      bg2 = palette.dark2;
      bg3 = palette.dark3;
      bg4 = palette.dark4;
      gray = palette.gray_245;
      fg0 = palette.light0;
      fg1 = palette.light1;
      fg2 = palette.light2;
      fg3 = palette.light3;
      fg4 = palette.light4;
      fg4_256 = palette.light4_256;
      red = palette.bright.red;
      green = palette.bright.green;
      yellow = palette.bright.yellow;
      blue = palette.bright.blue;
      purple = palette.bright.purple;
      aqua = palette.bright.aqua;
      orange = palette.bright.orange;
    };
    soft.dark =
      dark
      // {
        bg0 = palette.soft.dark0;
      };
    hard.dark =
      dark
      // {
        bg0 = palette.hard.dark0;
      };
    light = {
      bg0 = palette.light0;
      bg1 = palette.light1;
      bg2 = palette.light2;
      bg3 = palette.light3;
      bg4 = palette.light4;
      gray = palette.gray_244;
      fg0 = palette.dark0;
      fg1 = palette.dark1;
      fg2 = palette.dark2;
      fg3 = palette.dark3;
      fg4 = palette.dark4;
      fg4_256 = palette.dark4_256;
      red = palette.faded.red;
      green = palette.faded.green;
      yellow = palette.faded.yellow;
      blue = palette.faded.blue;
      purple = palette.faded.purple;
      aqua = palette.faded.aqua;
      orange = palette.faded.orange;
    };
    soft.light =
      light
      // {
        bg0 = palette.soft.light0;
      };
    hard.light =
      light
      // {
        bg0 = palette.hard.light0;
      };
  };
in {inherit colors palette;}

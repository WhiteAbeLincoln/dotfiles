{ bg, bg1, bg2, fg, red, yellow, green, gray, blue, orange }:

{
  options = {
    ## Gruvbox Dark
    notification-error-bg       = bg; # bg
    notification-error-fg       = red; # bright:red
    notification-warning-bg     = bg; # bg
    notification-warning-fg     = yellow; # bright:yellow
    notification-bg             = bg; # bg
    notification-fg             = green; # bright:green

    completion-bg               = bg2; # bg2
    completion-fg               = fg; # fg
    completion-group-bg         = bg1; # bg1
    completion-group-fg         = gray; # gray
    completion-highlight-bg     = blue; # bright:blue
    completion-highlight-fg     = bg2; # bg2

    # Define the color in index mode
    index-bg                    = bg2; # bg2
    index-fg                    = fg; # fg
    index-active-bg             = blue; # bright:blue
    index-active-fg             = bg2; # bg2

    inputbar-bg                 = bg; # bg
    inputbar-fg                 = fg; # fg

    statusbar-bg                = bg2; # bg2
    statusbar-fg                = fg; # fg

    highlight-color             = yellow; # bright:yellow
    highlight-active-color      = orange; # bright:orange

    default-bg                  = bg; # bg
    default-fg                  = fg; # fg
    render-loading              = true;
    render-loading-bg           = bg; # bg
    render-loading-fg           = fg; # fg

    # Recolor book content's color
    recolor-lightcolor          = bg; # bg
    recolor-darkcolor           = fg; # fg
    recolor                     = "true";
    # set recolor-keephue             true      # keep original color
  };
}

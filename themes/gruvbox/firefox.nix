{ color-scheme, density }:
{ bg0, bg1, bg2, fg0, fg1, fg2, fg3, fg4, green, blue, ... }:

let
  background = bg0;
  foreground = fg1;
  gray = fg4;
  border-color = gray;
  selected-background = bg2;
  selected-foreground = fg0;
in
  {
    inherit color-scheme density;
    # see https://developer.mozilla.org/en-US/docs/Tools/DevToolsColors for devtools theming
    userChrome = ''
      :root {
        --toolbar-color: ${foreground} !important;
        --toolbar-bgcolor: ${background} !important;
        --lwt-toolbarbutton-icon-fill: ${foreground} !important;
      }
      :root:-moz-lwtheme[lwt-toolbar-field-focus-brighttext] {
        --urlbar-popup-action-color: ${green} !important;
        --urlbar-popup-url-color: ${blue} !important;
      }
      #navigator-toolbox {
        font-family: Hack, monospace !important;
      }

      /* BEGIN TOOLBAR */
      #nav-bar.browser-toolbar:not(.titlebar-color) {
        border-top: 1px solid ${bg2};
      }
      /* END TOOLBAR */

      /* BEGIN URL BAR */
      #urlbar {
        --urlbar-separator-color: ${border-color};
      }
      #urlbar > #urlbar-background {
        background-color: ${background} !important;
        color: ${foreground} !important;
      }

      /* change border color when focused */
      #urlbar[focused="true"] > #urlbar-background {
        border: 2px solid ${border-color} !important;
      }
      #urlbar:not([focused="true"]) > #urlbar-background {
        border: none !important;
      }
      /* END URL BAR */

      /* BEGIN POPUPS */
      panel panelview {
        border: 2px solid ${border-color};
        background-color: ${background} !important;
        color: ${foreground};
      }

      image.panel-arrow {
        display: none;
      }
      /* END POPUPS */

      /* BEGIN TABS */
      #navigator-toolbox .toolbar-items {
        background-color: ${background};
      }
      #navigator-toolbox .tab-background {
        background-color: ${background} !important;
      }
      #navigator-toolbox .tabbrowser-tab .tab-label {
        color: ${foreground} !important;
      }

      #navigator-toolbox .tab-background[selected='true'] {
        background: ${selected-background} !important;
      }
      #navigator-toolbox .tabbrowser-tab[selected='true'] .tab-label {
        color: ${selected-foreground} !important;
      }
      /* END TABS */
    '';
  }

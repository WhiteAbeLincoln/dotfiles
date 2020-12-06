pkgs:

{
  customPaneNavigationAndResize = true;
  clock24 = true;
  escapeTime = 0; # fix for laggy escape key in vim
  keyMode = "vi";
  newSession = true;
  plugins = with pkgs.tmuxPlugins; [
    sensible
    sessionist
    pain-control
    yank
  ];
  secureSocket = true;
  terminal = "xterm-256color"; # support 24-bit color in termite
  extraConfig = ''
    set -ga terminal-overrides ",xterm-termite:Tc"
    # only set C-f as prefix if not in ssh session
    if 'test ! -n "$SSH_TTY"' \
      "unbind C-b; set -g prefix C-f"
    # mouse mode keys
    bind m set -g mouse on
    bind M set -g mouse off
    set -g mouse on
    bind -T copy-mode-vi 'v' send -X begin-selection
    set -g automatic-rename on
    setw -g monitor-activity on
    set -g visual-activity on
  '';
}

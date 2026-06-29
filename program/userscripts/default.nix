{...}: {
  imports = [./module.nix];

  programs.userscripts = {
    enable = true;

    # Userscripts written to every enabled target. The attribute name is the
    # destination filename verbatim — include the `.user.js` suffix. Each entry sets
    # exactly one of `text` (inline) or `source` (a file).
    scripts = {
      # "example.user.js".text = ''
      #   // ==UserScript==
      #   // @name    Example
      #   // @match   *://*/*
      #   // ==/UserScript==
      #   console.log("hello");
      # '';
      # "from-file.user.js".source = ./scripts/from-file.user.js;
    };

    # On darwin a `userscripts-safari` target is registered automatically, pointing at
    # the Userscripts Safari extension's scripts directory. Add more directory targets
    # here, override the default directory, or disable it with:
    #   targets.userscripts-safari.enable = false;
    # targets.firefox-sync.directory = "${config.home.homeDirectory}/userscripts";
  };
}

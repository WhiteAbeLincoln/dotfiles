{lib, ...}: {
  homeManager = {pkgs, ...}: {
    programs.bat.enable = lib.mkDefault true;
    programs.eza.enable = lib.mkDefault true;
    home.packages = with pkgs; [
      # find alternative (not command line compatible)
      fd
      ripgrep
      # a system monitor, alternative to top https://github.com/ClementTsang/bottom
      bottom
      # a modern alternative to curl https://github.com/ducaale/xh
      xh
      lazygit
    ];
    programs.fish.shellAliases = {
      # docker = "podman";
      # cat replacement
      cat = "bat --paging=never";
      # ls replacement https://github.com/eza-community/eza
      ll = "eza --classify --long --all --header --git --hyperlink";
      # start with depth 2 by default, luckily eza allows overriding
      # the level flag by providing it again, so I can tack on another
      # when using the alias to go deeper.
      # in many directories it runs into max filedescriptor limits
      # if we run without a depth limit so 2 is a reasonable default.
      # I can always override with a big depth if it matters.
      tree = "eza --classify --long --git --hyperlink --tree --level=2";
      ls = "eza --classify --hyperlink";
    };
  };
}

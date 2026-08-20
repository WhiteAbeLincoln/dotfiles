{inputs, ...}: {
  imports = [./ai-agents];
  homeManager = {pkgs, ...}: {
    home.packages = [
      pkgs.nil
      pkgs.diff2html-cli
      pkgs.difftastic
      inputs.git-different.packages.${pkgs.system}.default
      pkgs.imagemagick
    ];
  };
}

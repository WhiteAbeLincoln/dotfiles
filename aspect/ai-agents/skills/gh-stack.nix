{pkgs, ...}: let
  stack = pkgs.unstable.gh-stack;
in {
  programs.gh = {
    enable = true;
    package = pkgs.unstable.gh;
    extensions = [stack];

    # Preserve existing Git authentication rather than enabling a new helper.
    gitCredentialHelper.enable = false;
  };

  # Keep the CLI and its complete skill (including references) at the same version.
  programs.ai-agents.skills.gh-stack = "${stack}/share/skills/gh-stack/gh-stack";
}

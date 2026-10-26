{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./module.nix
    ./skills/jira.nix
    ./skills/fastly.nix
  ];
  programs.ai-agents = {
    enable = lib.mkDefault true;

    # Directory holding AGENTS.md plus the context docs it references via @ctx/.
    contextDir = ./agents;

    skills.work-issue = ./skills/work-issue;

    claude-code = {
      enable = true;
      package = pkgs.llm-agents.claude-code;
    };

    codex = {
      enable = true;
      package = pkgs.llm-agents.codex;
    };

    pi = {
      enable = true;
      package = pkgs.llm-agents.pi;
      extensions.ask = ./pi/ask.ts;
      settings.packages = ["npm:pi-vim@0.14.2"];
    };
  };
}

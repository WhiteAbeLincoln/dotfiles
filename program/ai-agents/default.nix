{pkgs, ...}: {
  imports = [
    ./skills/jira.nix
  ];
  programs.ai-agents = {
    enable = true;

    # Directory holding AGENTS.md plus the context docs it references via @ctx/.
    contextDir = ./agents;

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
      settings = {};
    };
  };
}

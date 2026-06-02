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
      package = pkgs.unstable.claude-code;
    };

    codex = {
      enable = true;
      package = pkgs.codex;
    };

    pi = {
      enable = true;
      package = pkgs.pi-coding-agent;
      settings = {};
    };
  };
}

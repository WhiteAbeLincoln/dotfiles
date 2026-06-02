{pkgs, ...}: {
  home.packages = [
    pkgs.unstable.acli # atlassian cli, for Jira and Confluence management
    # Markdown → Atlassian Document Format converter. Pipeline:
    #   mdadf desc.md > /tmp/desc.adf.json
    #   acli jira workitem edit --key STX-x --description-file /tmp/desc.adf.json --yes
    pkgs.mdadf
  ];
  programs.ai-agents.skills.jira = pkgs.fetchgit {
    url = "https://github.com/chenhunghan/jira-skill";
    rev = "bf9c95e275ade1765d463106fa43ed8dc7104eea";
    hash = "sha256-gBbSt0ChkzTedE6CVEabP47ZHE6w8R0wPbXf//PKaNw=";
  };
}

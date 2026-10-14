{
  pkgs,
  lib,
  ...
}: let
  repo = pkgs.fetchgit {
    url = "https://github.com/fastly/fastly-agent-toolkit";
    rev = "6db70eaf54ba885d56091861224987d6cf06c429";
    hash = "sha256-m7g0RAsyHMTbEFnr6GkfUWZt+3StwWUGmD61eTgwklo=";
  };
in {
  programs.ai-agents.skills = lib.genAttrs [
    "falco"
    "fastlike"
    "fastly"
    "fastly-cli"
    "fastly-fiddle"
    "fastly-ngwaf"
    "fastly-reference-architectures"
    "fastly-stats"
    "viceroy"
    "xvcl"
  ] (name: "${repo}/skills/${name}");
}

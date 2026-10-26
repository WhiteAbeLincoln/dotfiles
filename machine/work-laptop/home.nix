{
  pkgs,
  lib,
  ...
}: let
  aws = lib.getExe pkgs.unstable.awscli2;
  ensureBedrockCredentials = pkgs.writeShellScript "ensure-bedrock-credentials" ''
    # Resolve (and refresh) cached credentials without an STS request on every launch.
    if ! ${aws} configure export-credentials --profile=bedrock >/dev/null 2>&1; then
      echo "Bedrock credentials unavailable; signing in with AWS SSO..." >&2
      ${aws} sso login --profile=bedrock || exit "$?"
      ${aws} configure export-credentials --profile=bedrock >/dev/null || exit "$?"
    fi
  '';
in {
  programs.ai-agents.pi.settings = {
    defaultProvider = "amazon-bedrock";
    defaultModel = "us.openai.gpt-6.1-sol";
  };

  programs.ai-agents.pi.package = lib.mkForce (pkgs.symlinkJoin {
    name = "pi-bedrock";
    paths = [pkgs.llm-agents.pi];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/pi" \
        --set AWS_PROFILE bedrock \
        --run ${lib.escapeShellArg "${ensureBedrockCredentials} || exit \"$?\""}
    '';
    meta = pkgs.llm-agents.pi.meta;
  });

  # TODO: move these deps to flakes/dev shells in project directories
  home.packages = [
    pkgs.unstable.rustup
    pkgs.unstable.nodejs_24
    pkgs.unstable.go
    pkgs.unstable.terraform
    pkgs.unstable.gh
    pkgs.unstable.awscli2
    # Colima manages the daemon; the Docker CLI is packaged separately.
    pkgs.unstable.colima
    pkgs.unstable.docker-client
  ];
  programs.ghostty.enable = true;
  # use fastly email for all repos not covered
  # by aspect/git/home.nix includes
  programs.git.settings.user.email = "abe.white@fastly.com";
}

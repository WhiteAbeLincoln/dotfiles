{
  writeShellApplication,
  coreutils,
  findutils,
  curl,
}:
writeShellApplication {
  name = "audit-agent-access";
  runtimeInputs = [coreutils findutils curl];
  # `sudo` is a setuid system wrapper (not a nix package) and is reached via the
  # inherited PATH; this script is meant to run on globalhawk as the operator.
  text = builtins.readFile ./audit-agent-access.sh;
}

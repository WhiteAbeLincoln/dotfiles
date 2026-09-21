# TODO: nix-index works under darwin too
# but should we have it enabled for both
{
  inputs,
  lib,
  ...
}: {
  homeManager = {...}: {
    imports = [
      inputs.nix-index-database.hmModules.nix-index
    ];
    programs.nix-index.enable = lib.mkDefault true;
    programs.nix-index-database.comma.enable = lib.mkDefault true;

    # comma execs the `nix` baked into its own closure (nixpkgs patches the
    # store path in), not the one on PATH, so a bare `nixpkgs#` ref resolves
    # through *that* nix's global registry — the nixpkgs-unstable channel
    # tarball — instead of ours. Left alone, `,` fetches a third nixpkgs and
    # runs builds matching neither the system nor `nix run nixpkgs#...`.
    # Pinning the flake ref makes comma independent of any registry.
    # (Ignored if NIX_PATH is ever set: comma then takes its `-f <nixpkgs>`
    # channel path instead. We deliberately do not set NIX_PATH.)
    home.sessionVariables.COMMA_NIXPKGS_FLAKE = "${inputs.nixpkgs}";
  };
}

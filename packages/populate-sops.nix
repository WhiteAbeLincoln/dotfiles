{
  writeShellApplication,
  python3,
  sops,
}: let
  python = python3.withPackages (packages: [packages.argon2-cffi]);
in
  writeShellApplication {
    name = "populate-sops";
    runtimeInputs = [
      python
      sops
    ];
    text = ''
      exec ${python}/bin/python3 ${./populate-sops.py} "$@"
    '';
  }

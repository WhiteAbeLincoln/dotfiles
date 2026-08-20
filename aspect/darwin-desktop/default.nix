{...}: {
  darwin = {
    imports = [
      ./defaults-writer.nix
      ./module.nix
    ];
  };
}

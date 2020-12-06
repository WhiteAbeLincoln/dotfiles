{ ... }:

let
  nurPkg = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/07021663631838d34b462598e80ff066225d501e.tar.gz";
    sha256 = "1xphv9mkdx7pnbp25mqvvzmal5r2qfrmvm3xppw4y677m7nd1pic";
  });
  abes-xmonadPkg = import ./xmonad;
in

{
  nixpkgs.config.packageOverrides = pkgs: {
    nur = nurPkg {
      inherit pkgs;
    };
    abes-xmonad = abes-xmonadPkg {
      inherit pkgs;
    };
  };
}

self: super: {
  skhd = super.skhd.overrideAttrs (o: rec {
    makeFlags = [];
    postInstall = ''
    mkdir -p $out/Library/LaunchDaemons
    mkdir -p $out/bin
    cp ./bin/skhd $out/bin/skhd
    cp ${./org.nixos.skhd.plist} $out/Library/LaunchDaemons/org.nixos.skhd.plist
    substituteInPlace $out/Library/LaunchDaemons/org.nixos.skhd.plist --subst-var out
    '';
  });
}

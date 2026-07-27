{
  lib,
  buildGoModule,
  dockerTools,
  cacert,
}: let
  version = "1.0.2";
  exporter = buildGoModule {
    pname = "plex-exporter";
    inherit version;
    src = ./.;
    vendorHash = null;
    subPackages = ["."];
    doCheck = true;
    ldflags = ["-s" "-w"];
    meta = {
      description = "Privacy-preserving aggregate Plex metrics exporter";
      license = lib.licenses.mit;
      mainProgram = "plex-exporter";
    };
  };
in
  dockerTools.buildLayeredImage {
    name = "localhost/plex-exporter";
    tag = version;
    contents = [exporter cacert];
    config = {
      Entrypoint = ["/bin/plex-exporter"];
      User = "65532:65532";
      ExposedPorts."9100/tcp" = {};
    };
  }

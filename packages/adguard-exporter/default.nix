{
  lib,
  buildGoModule,
  dockerTools,
  cacert,
}: let
  version = "1.0.2";
  exporter = buildGoModule {
    pname = "adguard-exporter";
    inherit version;
    src = ./.;
    vendorHash = null;
    subPackages = ["."];
    doCheck = true;
    ldflags = ["-s" "-w"];
    meta = {
      description = "Privacy-preserving aggregate AdGuard Home metrics exporter";
      license = lib.licenses.mit;
      mainProgram = "adguard-exporter";
    };
  };
in
  dockerTools.buildLayeredImage {
    name = "localhost/adguard-exporter";
    tag = version;
    contents = [exporter cacert];
    config = {
      Entrypoint = ["/bin/adguard-exporter"];
      User = "65532:65532";
      ExposedPorts."9100/tcp" = {};
    };
  }

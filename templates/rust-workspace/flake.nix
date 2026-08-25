{
  description = "A multi-crate Rust workspace";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    crane,
    rust-overlay,
  }: let
    supportedSystems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-linux"
    ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [rust-overlay.overlays.default];
      };
    craneLibFor = system: let
      pkgs = pkgsFor system;
      rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
    in
      (crane.mkLib pkgs).overrideToolchain rustToolchain;
  in {
    checks = forAllSystems (
      system: let
        craneLib = craneLibFor system;
        src = craneLib.cleanCargoSource ./.;
        commonArgs = {
          inherit src;
          pname = "project";
          version = "0.1.0";
          strictDeps = true;
          cargoExtraArgs = "--workspace --locked";
        };
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;
      in {
        project = craneLib.buildPackage (
          commonArgs
          // {
            inherit cargoArtifacts;
            doCheck = false;
          }
        );

        cargo-clippy = craneLib.cargoClippy (
          commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          }
        );

        cargo-fmt = craneLib.cargoFmt {
          inherit src;
        };

        cargo-nextest = craneLib.cargoNextest (
          commonArgs
          // {
            inherit cargoArtifacts;
            partitions = 1;
            partitionType = "count";
            cargoNextestPartitionsExtraArgs = "--no-tests=pass";
          }
        );
      }
    );

    packages = forAllSystems (system: {
      default = self.checks.${system}.project;
      project = self.checks.${system}.project;
    });

    apps = forAllSystems (system: {
      default = {
        type = "app";
        program = "${self.packages.${system}.project}/bin/project";
      };
    });

    devShells = forAllSystems (
      system: let
        pkgs = pkgsFor system;
        rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        devRustToolchain = rustToolchain.override (previous: {
          extensions =
            previous.extensions
            ++ [
              "rust-analyzer"
              "rust-src"
            ];
        });
        craneLib = (crane.mkLib pkgs).overrideToolchain devRustToolchain;
      in {
        default = craneLib.devShell {
          checks = self.checks.${system};
          packages = [pkgs.cargo-nextest];
        };
      }
    );
  };
}

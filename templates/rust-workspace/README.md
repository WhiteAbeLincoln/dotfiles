# project

A multi-crate Rust workspace built with crane and a pinned Rust toolchain.

Rename the `project` and `project-cli` packages, the `project` binary, and the
matching flake attributes before beginning development.

## Development

Enter the Nix development environment:

```console
nix develop
```

The Rust channel is declared in `rust-toolchain.toml` and supplied by
`oxalica/rust-overlay`.
The development shell also includes `rust-analyzer` and `rust-src` for editor
integration.

Run the workspace checks:

```console
cargo nextest run --workspace
cargo clippy --workspace --all-targets -- --deny warnings
cargo fmt --all --check
```

Build or run the CLI through Nix:

```console
nix build
nix run
```

`nix flake check` runs the build, clippy, formatting, and nextest checks. Crane
builds workspace dependencies in a separate derivation so they remain cached
when only project source files change.

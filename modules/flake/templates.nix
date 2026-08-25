{...}: {
  flake.templates.project-rust-workspace = {
    path = ../../templates/rust-workspace;
    description = "A multi-crate Rust workspace built with crane and rust-overlay";
  };
}

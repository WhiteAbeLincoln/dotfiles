self: super: {
  yabai = super.callPackage ./default.nix {
    # this is usually done in nixpkgs top-level file for os-specific apps
    inherit (super.darwin.apple_sdk.frameworks) Carbon Cocoa ScriptingBridge;
  };
}

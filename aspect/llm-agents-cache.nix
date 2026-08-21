{...}: let
  settings = {
    extra-substituters = ["https://cache.numtide.com"];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };
in {
  nixos.nix.settings = settings;
  darwin.determinateNix.customSettings = settings;
}

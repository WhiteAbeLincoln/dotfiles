{ config, lib, pkgs, ... }:

{
  programs.emacs.enable = true;
  # services.emacs.enable = pkgs.stdenv.isLinux;
  home.file.".spacemacs".source = ./spacemacs-new;
  # home.file.".emacs.d" = {
  #   source = pkgs.fetchFromGitHub {
  #     owner = "syl20bnr";
  #     repo = "spacemacs";
  #     rev = "c483818";
  #     sha256 = "00wpvgrhilr4j32icm3750fvx745cvgxlx73afyp1cshvx0dz621";
  #   };
  #   recursive = true;
  # };
  # TODO: install spacemacs: git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
  # see https://gitlab.com/Myhlamaeus/nixos-config/-/blob/04368937400eb82d5c1155741e17cbc68d94c8cd/users/Myhlamaeus/home/editors/emacs.nix
  # see https://github.com/Mic92/dotfiles/blob/3c7dad7b342570a18e35f6972f0f954cb263bd64/nixpkgs-config/modules/emacs/default.nix#L47
}

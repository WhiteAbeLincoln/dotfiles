#!/bin/sh

OSTYPE="$(uname -s)"
DRY_RUN=0

trap '[ "$PWD" = "/tmp" ] && popd; trap - EXIT; exit' EXIT

execute() {
  printf '%s\n' "COMMAND: $*"

  if [ $DRY_RUN -eq 1 ]; then
    return 0
  fi

  eval "$@"
}

islinux() {
  [ $OSTYPE = "Linux" ]
}

isdarwin() {
  [ $OSTYPE = "Darwin" ]
}

iswsl() {
  islinux && [ -f /proc/version ] && [ "$(< /proc/version)" = *[Mm]icrosoft* ]
}

nixinstalled() {
  [ -d /nix ]
}

BASIC_DARWIN_CONFIG='{ config, pkgs, ... }:\n\n{ imports = [ ./simple-darwin.nix ]; }\n'

install_nixdarwin() {
  if isdarwin; then
    printf '%s' "Installing nix-darwin. Continue [y/N]: "
    read -r cont
    if [ "$cont" = "y" ]; then
      execute "printf '%b\n' '$BASIC_DARWIN_CONFIG' > '$HOME/.nixpkgs/darwin-configuration.nix'"
      execute "sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.old"

      # install nix-darwin
      execute 'nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer'
      execute './result/bin/darwin-installer'

      printf '%s\n' "Remember to open a new shell, edit ~/.nixpkgs/darwin-configuration.nix and run darwin-rebuild switch"
    else
      printf '%s\n' "Skipping nix-darwin install..."
    fi
  fi
}

install_homemanager() {
  printf '%s' "Installing home-manager. Continue [y/N]: "
  read -r cont
  if [ "$cont" = "y" ]; then
    # install system home-manager if we are in nixos or macos
    # TODO: detect nixos
    if isdarwin; then
      printf '%s\n' "Installing system home-manager"
      execute 'sudo -i nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager'
      execute 'sudo -i nix-channel --update'
    else
      printf '%s\n' "Installing standalone home-manager"
      execute 'nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager'
      execute 'nix-channel --update'

      export NIX_PATH="$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}"
      execute 'nix-shell "<home-manager>" -A install'
    fi
  else
    printf '%s\n' "Skipping home-manager install..."
  fi
}

install_nix() {
  if nixinstalled; then
    printf '%s\n' "Nix is already installed. Skipping..."
    return 0
  fi

  if iswsl; then
    printf '%s' "Installing singleuser nix. Continue [y/N]: "
  else
    printf '%s' "Installing multiuser nix. Continue [y/N]: "
  fi

  read -r cont
  if [ "$cont" = "y" ]; then
    execute 'curl -L https://nixos.org/nix/install > /tmp/install-nix'
    # install nix
    if iswsl; then
      execute 'sh /tmp/install-nix --no-daemon'
    elif islinux; then
      execute 'sh /tmp/install-nix --daemon'
    elif isdarwin; then
      execute 'sh /tmp/install-nix'
    else
      printf >&2 '%s\n' "System $OSTYPE not supported"
      exit 1
    fi
    printf '%s\n' "Restart your shell and rerun this script."
    exit 0
  else
    printf '%s\n' "Cancelling install..."
    exit 0
  fi
}

do_install() {
  execute 'pushd /tmp'

  install_nix
  install_homemanager
  install_nixdarwin
}

do_uninstall() {
  if isdarwin; then
    printf '%s' "Uninstalling nix-darwin. Continue [y/N]: "
    read -r cont
    if [ "$cont" = "y" ]; then
      execute 'pushd /tmp'
      execute 'nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A uninstaller'
      execute './result/bin/darwin-uninstaller'
    else
      printf '%s\n' "Skipping nix-darwin uninstall..."
    fi

    printf '%s' "Uninstalling nix. Continue [y/N]: "
    read -r cont
    if [ "$cont" = "y" ]; then
      printf '%s\n' "Restoring original shell rc files"
      execute 'sudo mv /etc/zshrc.backup-before-nix /etc/zshrc'
      execute 'sudo mv /etc/bashrc.backup-before-nix /etc/bashrc'
      execute 'sudo mv /etc/zprofile.orig /etc/zprofile'

      printf '%s\n' "Removing nix store volume from fstab"
      execute 'sudo vifs'

      printf '%s\n' "Remove nix from /etc/synthetic.conf"
      execute 'sudo vim /etc/synthetic.conf'

      printf '%s\n' "Stopping nix daemon service"
      execute 'sudo launchctl unload /Library/LaunchDaemon/org.nixos.nix-daemon.plist'
      execute 'sudo rm /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
      execute 'sudo launchctl unload /Library/LaunchDaemons/org.nixos.activate-system.plist'
      execute 'sudo rm /Library/LaunchDaemons/org.nixos.activate-system.plist'

      printf '%s\n' "Removing nix files"
      execute 'sudo rm -rf /etc/nix /var/root/.nix-profile /var/root/.nix-defexpr /var/root/.nix-channels ~/.nix-profile ~/.nix-defexpr ~/.nix-channels'

      printf '%s\n' "Removing nixbld groups"
      execute 'sudo dscl . delete /Groups/nixbld'
      for i in $(seq 1 32); do
        execute "sudo dscl . -delete /Users/_nixbld$i"
      done

      printf '%s\n' "Removing nix store volume"
      execute 'sudo diskutil apfs deleteVolume /nix'
      execute 'sudo rm -rf /nix/'

      printf '%s' "Rebooting. Continue [y/N]: "
      read -r cont
      if [ "$cont" = "y" ]; then
        execute 'sudo reboot'
      else
        printf '%s\n' "Uninstall complete. Reboot needed"
        exit 0
      fi
    else
      printf '%s\n' "Cancelling uninstall..."
      exit 0
    fi
  elif islinux; then
    echo "TODO: uninstall linux"
  else
    printf '%s\n' "System $OSTYPE not supported"
    exit 1
  fi
}

show_help() {
  echo "installer.sh [-h|--help] [--dry-run] (install | uninstall)"
}

ACTION=

while :; do
  case $1 in
    -h|--help)
      show_help
      exit
      ;;
    --dry-run)
      DRY_RUN=1
      echo "---Dry Run---"
      ;;
    install)
      ACTION="install"
      ;;
    uninstall)
      ACTION="uninstall"
      ;;
    *)
      break
  esac

  shift
done

if [ "$ACTION" = "install" ]; then
  do_install
elif [ "$ACTION" = "uninstall" ]; then
  do_uninstall
fi

#!/bin/sh

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)"
OSTYPE="$(uname -s)"
DRY_RUN=0
INTERACTIVE=0
LAST_DIR=""

trap '[ "$PWD" = "/tmp" ] && popd; trap - EXIT; exit' EXIT

command -v pushd >/dev/null 2>&1 || {
  pushd() {
    if [ -z "$LAST_DIR" ]; then
      LAST_DIR="$PWD"
    else
      # we assume newlines won't appear in paths
      LAST_DIR="$PWD
$LAST_DIR"
    fi
    cd "$1" || exit 1
  }

  popd() {
    if [ -z "$LAST_DIR" ]; then
      echo "directory stack empty"
      exit 1
    else
      cd "${LAST_DIR%%
*}" || exit 1
      LAST_DIR="${LAST_DIR#*
}"
    fi
  }
}

prompt() {
  printf '\e[1;34mContinue\e[0m [y/N]: '
  cont=''
  read -r cont
  [ "$cont" = "y" ]
}

execute() {
  printf '\e[37m%s\e[0m\n' "COMMAND: $*"

  if [ $INTERACTIVE -eq 1 ]; then
    prompt || {
      echo "Command execution rejected. Quitting..."
      exit 1
    }
  fi

  if [ $DRY_RUN -eq 1 ]; then
    return 0
  fi

  "$@"
}

islinux() {
  [ "$OSTYPE" = "Linux" ]
}

isdarwin() {
  [ "$OSTYPE" = "Darwin" ]
}

iswsl() {
  islinux && [ -f /proc/version ] && grep ".*[Mm]icrosoft.*" /proc/version >/dev/null
}

isnixos() {
  islinux && [ -f /etc/NIXOS ]
}

nixinstalled() {
  [ -d /nix ]
}

BASIC_DARWIN_CONFIG='{ config, pkgs, ... }:\n\n{ imports = [ ./simple-darwin.nix ]; }\n'

install_nixdarwin() {
  echo "Installing nix-darwin"
  prompt || {
    printf '%s\n' "Skipping nix-darwin install..."
    return 0
  }

  execute eval "printf '%b\n' '$BASIC_DARWIN_CONFIG' > '$HOME/.nixpkgs/darwin-configuration.nix'"
  execute sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.old

  # install nix-darwin
  execute nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
  execute ./result/bin/darwin-installer

  printf '%s\n' "Remember to open a new shell, edit ~/.nixpkgs/darwin-configuration.nix and run darwin-rebuild switch"
}

install_homemanager() {
  echo "Installing home-manager"
  prompt || {
    printf '%s\n' "Skipping home-manager install..."
    return 0
  }

  # install system home-manager if we are in nixos or macos
  if isdarwin || isnixos; then
    printf '%s\n' "Installing system home-manager"
    execute sudo -i nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    execute sudo -i nix-channel --update
  else
    printf '%s\n' "Installing standalone home-manager"
    execute nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    execute nix-channel --update

    export NIX_PATH="$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}"
    execute nix-shell "<home-manager>" -A install
  fi
}

install_nix() {
  if nixinstalled; then
    printf '%s\n' "Nix is already installed. Skipping..."
    return 0
  fi

  if iswsl; then
    echo "Installing singleuser nix."
  else
    echo "Installing multiuser nix."
  fi

  prompt || {
    printf '%s\n' "Cancelling install..."
    exit 0
  }

  execute eval 'curl -L https://nixos.org/nix/install > /tmp/install-nix'
  # install nix
  if iswsl; then
    execute sh /tmp/install-nix --no-daemon
  elif islinux; then
    execute sh /tmp/install-nix --daemon
  elif isdarwin; then
    execute sh /tmp/install-nix
  else
    printf >&2 '%s\n' "System $OSTYPE not supported"
    exit 1
  fi
  printf '%s\n' "Restart your shell and rerun this script."
  exit 0
}

bootstrap_nixos() {
  echo "Bootstrapping nixos config"
  prompt || {
    echo "Skipping nixos boostrap..."
    return 0
  }

  printf '%s' "Provide machine config: "

  read -r host_dir
  while [ ! -d "$SCRIPT_DIR/machine/$host_dir" ]; do
    echo "Invalid machine. Machine must be the name of a directory in $SCRIPT_DIR/machine"
    echo "Options are: "
    # shellcheck disable=SC2038
    find "$SCRIPT_DIR/machine" -maxdepth 1 -mindepth 1 -type d -exec basename '{}' \;
    printf '%s' "Try again or quit with ctrl-c: "
    read -r host_dir
  done

  echo "Backing up existing nixos configuration in /etc/nixos."
  if prompt; then
    for f in /etc/nixos/*; do
      execute sudo mv "$f" "$f.bak"
    done

    echo "Creating configuration.nix for $host_dir"
    execute eval "echo '{...}: { imports = [ $SCRIPT_DIR/machine/$host_dir ]; }' | sudo tee /etc/nixos/configuration.nix"
  else
    echo "Skipping nixos boostrap..."
  fi

  echo "Adding nixos-hardware channel"
  if prompt; then
    execute sudo -i nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
    execute sudo -i nix-channel --update
  else
    echo "Skipping nixos-hardware channel..."
  fi
}

do_install() {
  execute pushd /tmp

  if isnixos; then
    bootstrap_nixos
  else
    install_nix
  fi
  install_homemanager
  if isdarwin; then
    install_nixdarwin
  fi
}

do_uninstall() {
  if isnixos; then
    echo "Cannot uninstall nixos in place. Quitting..."
    exit 1
  fi

  if isdarwin; then
    echo "Uninstalling nix-darwin"
    if prompt; then
      execute pushd /tmp
      execute nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A uninstaller
      execute ./result/bin/darwin-uninstaller
    else
      printf '%s\n' "Skipping nix-darwin uninstall..."
    fi

    echo "Uninstalling nix."
    if prompt; then
      printf '%s\n' "Restoring original shell rc files"
      execute sudo mv /etc/zshrc.backup-before-nix /etc/zshrc
      execute sudo mv /etc/bashrc.backup-before-nix /etc/bashrc
      execute sudo mv /etc/zprofile.orig /etc/zprofile

      printf '%s\n' "Removing nix store volume from fstab"
      execute sudo vifs

      printf '%s\n' "Remove nix from /etc/synthetic.conf"
      execute sudo vim /etc/synthetic.conf

      printf '%s\n' "Stopping nix daemon service"
      execute sudo launchctl unload /Library/LaunchDaemon/org.nixos.nix-daemon.plist
      execute sudo rm /Library/LaunchDaemons/org.nixos.nix-daemon.plist
      execute sudo launchctl unload /Library/LaunchDaemons/org.nixos.activate-system.plist
      execute sudo rm /Library/LaunchDaemons/org.nixos.activate-system.plist

      printf '%s\n' "Removing nix files"
      execute sudo rm -rf /etc/nix /var/root/.nix-profile /var/root/.nix-defexpr /var/root/.nix-channels ~/.nix-profile ~/.nix-defexpr ~/.nix-channels

      printf '%s\n' "Removing nixbld groups"
      execute sudo dscl . delete /Groups/nixbld
      for i in $(seq 1 32); do
        execute sudo dscl . -delete "/Users/_nixbld$i"
      done

      printf '%s\n' "Removing nix store volume"
      execute sudo diskutil apfs deleteVolume /nix
      execute sudo rm -rf /nix/

      echo "Rebooting."
      if prompt; then
        execute sudo reboot
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
    -i|--interactive)
      INTERACTIVE=1
      echo "---Interactive---"
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
else
  show_help
  exit 1
fi

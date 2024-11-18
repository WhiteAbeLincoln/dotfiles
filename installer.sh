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
  if [ "$1" = "eval" ]; then
    EVAL="1"
    shift
  else
    EVAL="0"
  fi
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

  if [ "$EVAL" = "1" ]; then
    eval "$1"
  else
    "$@"
  fi
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
  islinux && grep NixOS /etc/os-release >/dev/null
}

nixinstalled() {
  [ -d /nix ]
}

install_nix() {
  if nixinstalled; then
    printf '%s\n' "Nix is already installed. Skipping..."
    return 0
  fi

  echo "Installing nix using the Determinate Systems installer."
  prompt || {
    printf '%s\n' "Cancelling install..."
    exit 0
  }

  execute eval 'curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install'
  printf '%s\n' "Restart your shell and rerun this script."
  exit 0
}

install_brew() {
  if command -v brew >/dev/null 2>&1; then
    echo "Homebrew is already installed. Skipping..."
    return 0
  fi

  if ! [ -f "/opt/homebrew/bin/brew" ]; then
    echo "Installing homebrew"
    prompt || {
      printf '%s\n' "Cancelling install..."
      return 0
    }

    # shellcheck disable=SC2016
    execute eval '/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
  fi
  # shellcheck disable=SC2016
  execute eval 'eval "$(/opt/homebrew/bin/brew shellenv)"'
}

install_darwin() {
  install_brew

  if ! [ -f "$SCRIPT_DIR/machine/$(hostname -s)/default.nix" ]; then
    echo "Missing nix-darwin machine configuration at $SCRIPT_DIR/machine/$(hostname -s)"
    exit 1
  fi

  echo "Installing nix-darwin flake with config from machine/$(hostname -s)"
  if prompt; then
    execute nix run nix-darwin -- switch --flake "$SCRIPT_DIR"
  fi
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
  if isnixos; then
    bootstrap_nixos
  else
    install_nix
  fi
  if isdarwin; then
    install_darwin
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
      execute sudo rm -i "/etc/*.backup-before-nix"

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

      printf '%s\n' "Removing user nix temp files"
      execute rm -rf "$HOME/.local/state/nix"
      execute rm -rf "$HOME/.local/state/home-manager"
      execute rm -rf "$HOME/Applications/Nix Apps"
      execute rm -rf "$HOME/Applications/Home Manager Apps"

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

do_switch() {
  if isdarwin; then
    execute darwin-rebuild switch --flake "$SCRIPT_DIR" "$@"
  elif isnixos; then
    execute nixos-rebuild switch --flake "$SCRIPT_DIR" "$@"
  else
    execute home-manager switch --flake "$SCRIPT_DIR" "$@"
  fi
}

do_build() {
  if isdarwin; then
    execute darwin-rebuild build --flake "$SCRIPT_DIR" "$@"
  elif isnixos; then
    execute nixos-rebuild build --flake "$SCRIPT_DIR" "$@"
  else
    execute home-manager
  fi
}

show_help() {
  echo "installer.sh [-h|--help] [--dry-run] [--home-ver <VERSION>] (install | uninstall | switch | build)"
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
    switch)
      ACTION="switch"
      ;;
    build)
      ACTION="build"
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
elif [ "$ACTION" = "switch" ]; then
  do_switch "$@"
elif [ "$ACTION" = "build" ]; then
  do_build "$@"
else
  show_help
  exit 1
fi

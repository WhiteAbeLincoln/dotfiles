{ stdenv
, writeScript
, plexRaw
, runCommandLocal
}:

let
  wrapperScript = writeScript "plex-run-script" ''
    #!${stdenv.shell}

    set -eu

    # The root path to our Plex installation
    root="${plexRaw}/Applications/Plex Media Server.app/Contents/MacOS"

    # Path to where we're storing our Plex data files. We default to storing
    # them in the user's home directory under the XDG-compatible location, but
    # allow overriding with an environment variable so the location can be
    # configured in our NixOS module.
    #
    # NOTE: the old version of Plex used /var/lib/plex as the default location,
    # but this package shouldn't assume that we're going to run Plex with the
    # ability to write to /var/lib, so using a subdirectory of $HOME when none
    # is specified feels less likely to have permission errors.
    if [[ -z "''${PLEX_DATADIR:-}" ]]; then
      PLEX_DATADIR="$HOME/Library/Application Support"
    fi
    if [[ ! -d "$PLEX_DATADIR" ]]; then
      echo "Creating Plex data directory: $PLEX_DATADIR"
      mkdir -p "$PLEX_DATADIR"
    fi

    rmSymLink() {
      echo "Removing $1 symlink: $2"
      rm "$2"
    }

    # If we have a plugin list (set by our NixOS module), we create plugins in
    # the data directory as expected. This is a colon-separated list of paths.
    if [[ -n "''${PLEX_PLUGINS:-}" ]]; then
      echo "Preparing plugin directory"

      pluginDir="$PLEX_DATADIR/Plex Media Server/Plug-ins"
      test -d "$pluginDir" || mkdir -p "$pluginDir"

      # First, remove all of the symlinks in the plugins directory.
      find "$pluginDir" -type l -exec printf "Removing plugin symlink: %s\n" '{}' \; -delete

      echo "Symlinking plugins"
      IFS=':' read -ra pluginsArray <<< "$PLEX_PLUGINS"
      for path in "''${pluginsArray[@]}"; do
        dest="$pluginDir/$(basename "$path")"

        if [[ ! -d "$path" ]]; then
          echo "Error symlinking plugin from $path: no such directory"
        elif [[ -d "$dest" || -L "$dest" ]]; then
          echo "Error symlinking plugin from $path to $dest: file or directory already exists"
        else
          echo "Symlinking plugin at: $path"
          ln -s "$path" "$dest"
        fi
      done
    fi

    if [[ -n "''${PLEX_SCANNERS:-}" ]]; then
      for scannerType in Common Movies Music Series; do
        echo "Preparing $scannerType scanners directory"

        scannerDir="$PLEX_DATADIR/Plex Media Server/Scanners/$scannerType"
        test -d "$scannerDir" || mkdir -p "$scannerDir"

        # First, remove all of the symlinks in the scanners directory.
        echo "Removing old symlinks"
        find "$scannerDir" -type l -exec printf "Removing scanner symlink: %s\n" '{}' \; -delete

        echo "Symlinking scanners"
        IFS=':' read -ra scannersArray <<< "$PLEX_SCANNERS"
        for path in "''${scannersArray[@]}"; do
          # The provided source should contain a 'Scanners' directory; symlink
          # from inside that.
          subpath="$path/Scanners/$scannerType"
          while IFS= read -r -d $'\0' file; do
            dest="$scannerDir/$(basename "$file")"

            if [[ -f "$dest" || -L "$dest" ]]; then
              echo "Error symlinking scanner from $file to $dest: file or directory already exists"
            else
              echo "Symlinking scanner at: $file"
              ln -s "$file" "$dest"
            fi
          done < <(find "$subpath" -type f -print0)
        done
      done
    fi

    exec "$root/Plex Media Server"
  '';
in
  runCommandLocal "plex-wrapper" {} ''
    mkdir "$out"
    ln -s "${wrapperScript}" "$out/plex-run-script"
  ''

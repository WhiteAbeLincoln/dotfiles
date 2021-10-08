{
  # install system apps in /Applications/Nix Apps by copying so Spotlight can index
  # also can do the same for home manager apps
  # adapted from nicknovitski https://github.com/nix-community/home-manager/issues/1341#issuecomment-778820334
  # and andreykaipov https://github.com/andreykaipov/home/blob/384292d67c76b4a0df2308f51f8eb39abb36725c/.config/nix/packages/default.nix#L35-L64
  installAppScript = sourceDir: destDir: ''
    IFS=$'\n'
    getContents() {
      find "$1" -type d -path "*/Contents/MacOS" | while read file; do
        printf '%s:%s\n' "$(tr -dc / <<< "$file" | wc -c)" "$file"
      done | sort -t':' -n | head -n 1 | cut -d':' -f2
    }
    hashApp() {
      local path="$(getContents "$1" 2>/dev/null)"; shift
      echo Hashing path $path >&2
      for bin in $(find "$path" -maxdepth 1 -perm /111 -type f 2>/dev/null); do
          md5sum "$bin" | cut -b-32
      done | md5sum | cut -b-32 | tee >(cat >&2)
    }

    sortbybasename() {
      while read -r data; do
        printf "%s\t%s\n" "$(basename "$data")" "$data"
      done | sort -t$'\t' -k1
    }

    trim() {
      local var="$*"
      # remove leading whitespace characters
      var="''${var#"''${var%%[![:space:]]*}"}"
      # remove trailing whitespace characters
      var="''${var%"''${var##*[![:space:]]}"}"
      printf '%s' "$var"
    }

    # first remove apps from destDir that exist, but don't exist in the store
    echo Copying Apps from "${sourceDir}" to "${destDir}"
    mkdir -p "${destDir}"
    NEW_APPS="$(find "${sourceDir}/Applications" -mindepth 1 -maxdepth 1 | sortbybasename)"
    OLD_APPS="$(find "${destDir}" -mindepth 1 -maxdepth 1 | sortbybasename)"
    # joins on OLD and NEW, keeping only those apps in OLD that do not exist in NEW
    ONLY_OLD="$(echo "$NEW_APPS" | join -t$'\t' -v 2 -o "1.2 2.2" - <(echo "$OLD_APPS"))"

    # trims in the case where ONLY_OLD contains only whitespace
    case $ONLY_OLD in
      (*[![:blank:]]*) ;;
      ("") ONLY_OLD="";;
      (*) ONLY_OLD=""
    esac

    if [ ! -z $ONLY_OLD ]; then
      echo Deleting Removed Apps...
      for app in $ONLY_OLD; do
        app="$(trim "$app")"
        echo Deleting $app...
        rm -rf "$app"
      done
      echo Finished Deleting Removed Apps...
    fi

    for app in $NEW_APPS; do
      name="$(echo "$app" | cut -f1)"
      app="$(echo "$app" | cut -f2)"
      echo Checking App "$name"

      src="$(/usr/bin/stat -f%Y "$app")"
      dst="${destDir}/$name"
      hash1="$(hashApp "$src")"
      hash2="$(hashApp "$dst")"
      if [ "$hash1" != "$hash2" ]; then
        echo "Current hash of '$name' differs from the Nix store's. Overwriting..."
        if [ -e "$dst" ]; then
          $DRY_RUN_CMD rm ''${VERSBOSE_ARG:+-v} -rf "$dst"
        fi
        $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$app" "${destDir}"
        $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "${destDir}/$name"
        echo "Done"
      fi
    done
  '';
  mkApplication = {
    name, appname ? name, version, src, description, homepage,
    installPhase ? (path: ''cp -pR * ${path}''), sourceRoot ? ".",
    lib, stdenv, undmg, unzip, ...
  }: stdenv.mkDerivation {
    name = "${name}-${version}";
    version = "${version}";
    src = src;
    buildInputs = [ undmg unzip ];
    sourceRoot = sourceRoot;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p "$out/Applications/${appname}.app"
    '' + (installPhase "$out/Applications/${appname}.app");
    meta = with lib; {
      description = description;
      homepage = homepage;
      platforms = platforms.darwin;
      maintainers = [
        "Abraham White <abelincoln.white@gmail.com>"
      ];
    };
  };
}

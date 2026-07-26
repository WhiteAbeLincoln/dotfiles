{
  writeShellApplication,
  docker-client,
}:
writeShellApplication {
  name = "libation-auth";
  runtimeInputs = [docker-client];
  text = ''
    usage() {
      echo "Usage: libation-auth ACCOUNT LOCALE"
    }

    if [ "$#" -eq 1 ] && { [ "$1" = "-h" ] || [ "$1" = "--help" ]; }; then
      usage
      exit 0
    fi

    if [ "$#" -ne 2 ]; then
      usage >&2
      exit 2
    fi

    account="$1"
    locale="$2"
    image="rmcrackan/libation:13.5.1@sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0"
    config="/data/Media/apps/libation/config"

    docker run --rm --interactive --tty \
      --user 994:994 \
      --volume "$config:/config" \
      --entrypoint /bin/bash \
      "$image" \
      -c '
        set -euo pipefail

        for file in Settings.json AccountsSettings.json; do
          if [ -f "/config/$file" ]; then
            cp "/config/$file" "$LIBATION_CONFIG_INTERNAL/$file"
          else
            printf "{}\n" > "$LIBATION_CONFIG_INTERNAL/$file"
          fi
        done

        /libation/LibationCli \
          login-external \
          --libationFiles "$LIBATION_CONFIG_INTERNAL" \
          --account "$1" \
          --locale "$2"

        destination="/config/.AccountsSettings.json.$$"
        cp "$LIBATION_CONFIG_INTERNAL/AccountsSettings.json" "$destination"
        mv "$destination" /config/AccountsSettings.json

        /libation/LibationCli \
          list-accounts \
          --libationFiles "$LIBATION_CONFIG_INTERNAL"
      ' \
      -- \
      "$account" \
      "$locale"
  '';
}

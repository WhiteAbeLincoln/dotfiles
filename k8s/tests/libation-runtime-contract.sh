#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
out="$(nix build --no-link --print-out-paths \
  "$repo_root#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage")"
manifest="$(find -L "$out/libation" -type f -name 'CronJob-libation-reconcile.yaml' -print -quit)"

test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

nix run nixpkgs#yq-go -- \
  '.spec.jobTemplate.spec.template.spec.containers[0].command[3]' \
  "$manifest" >"$test_root/run.sh"
nix run nixpkgs#yq-go -- \
  '.data."Settings.json"' \
  "$(find -L "$out/libation" -type f -name 'ConfigMap-libation-settings.yaml' -print -quit)" \
  >"$test_root/settings.json"

jq -e '
  .ImportEpisodes == false
  and .DownloadEpisodes == false
  and .AutoDownloadEpisodes == false
  and .FolderTemplate == "<title short> [<id>]"
  and .FileTemplate == "<title> [<id>]"
' "$test_root/settings.json" >/dev/null

mkdir "$test_root/config" "$test_root/config-internal" "$test_root/libation"
printf '%s\n' '{"token":"before"}' >"$test_root/config/AccountsSettings.json"
printf '%s\n' '{}' >"$test_root/config/Settings.json"

# Pull and execute the exact entrypoint selected by the rendered immutable image.
image="$(nix run nixpkgs#yq-go -- \
  '.spec.jobTemplate.spec.template.spec.containers[0].image' "$manifest")"
image="${image/:13.5.1@/@}"
nix run nixpkgs#skopeo -- --insecure-policy copy \
  "docker://$image" "dir:$test_root/image" >/dev/null
mkdir "$test_root/root"
for layer in "$test_root"/image/*; do
  if tar -tf "$layer" >/dev/null 2>&1; then
    tar -xf "$layer" -C "$test_root/root"
  fi
done
sed \
  -e "s#/libation/LibationCli scan#printf '%s\\\\n' '{\"token\":\"refreshed\"}' >'$test_root/config-internal/AccountsSettings.json'#" \
  -e 's#/libation/LibationCli liberate#true#' \
  "$test_root/root/libation/liberate.sh" >"$test_root/libation/liberate.sh"
chmod +x "$test_root/libation/liberate.sh"

# Execute the rendered wrapper against isolated paths. Only Audible network calls are
# replaced; the pinned entrypoint still performs its real inward config copy.
sed \
  -e "s#/libation/liberate.sh#bash $test_root/libation/liberate.sh#g" \
  -e "s#/config-internal#$test_root/config-internal#g" \
  -e "s#/config/#$test_root/config/#g" \
  "$test_root/run.sh" >"$test_root/isolated-run.sh"
LIBATION_CONFIG_DIR="$test_root/config" \
  LIBATION_CONFIG_INTERNAL="$test_root/config-internal" \
  LIBATION_DB_DIR="$test_root/config" \
  LIBATION_BOOKS_DIR="$test_root" \
  LIBATION_CREATE_DB=true \
  SLEEP_TIME=-1 \
  bash "$test_root/isolated-run.sh"

jq -e '.token == "refreshed"' "$test_root/config/AccountsSettings.json" >/dev/null

printf '%s\n' '{"token":"known-good"}' >"$test_root/config/AccountsSettings.json"
cp "$test_root/config/AccountsSettings.json" "$test_root/known-good-accounts.json"
sed \
  -e "s#/libation/LibationCli scan#printf '%s\\\\n' '{\"token\":\"invalid-session\"}' >'$test_root/config-internal/AccountsSettings.json'; exit 37#" \
  "$test_root/root/libation/liberate.sh" >"$test_root/libation/liberate.sh"

set +e
LIBATION_CONFIG_DIR="$test_root/config" \
  LIBATION_CONFIG_INTERNAL="$test_root/config-internal" \
  LIBATION_DB_DIR="$test_root/config" \
  LIBATION_BOOKS_DIR="$test_root" \
  LIBATION_CREATE_DB=true \
  SLEEP_TIME=-1 \
  bash "$test_root/isolated-run.sh"
status=$?
set -e

test "$status" -eq 37
cmp "$test_root/known-good-accounts.json" "$test_root/config/AccountsSettings.json"

sed \
  -e "s#/libation/LibationCli scan#printf '%s\\\\n' '{\"token\":\"unpersisted\"}' >'$test_root/config-internal/AccountsSettings.json'#" \
  -e 's#/libation/LibationCli liberate#true#' \
  "$test_root/root/libation/liberate.sh" >"$test_root/libation/liberate.sh"
sed \
  -e "s#cp $test_root/config-internal/AccountsSettings.json $test_root/config/AccountsSettings.json#cp $test_root/config-internal/AccountsSettings.json $test_root/missing/AccountsSettings.json#" \
  "$test_root/isolated-run.sh" >"$test_root/copy-back-failure-run.sh"

set +e
LIBATION_CONFIG_DIR="$test_root/config" \
  LIBATION_CONFIG_INTERNAL="$test_root/config-internal" \
  LIBATION_DB_DIR="$test_root/config" \
  LIBATION_BOOKS_DIR="$test_root" \
  LIBATION_CREATE_DB=true \
  SLEEP_TIME=-1 \
  bash "$test_root/copy-back-failure-run.sh"
status=$?
set -e

test "$status" -ne 0
echo "libation runtime contract: account state persists only after successful reconciliation"

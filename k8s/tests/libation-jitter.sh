#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
out="$(nix build --no-link --print-out-paths \
  "$repo_root#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage")"
manifest="$(find -L "$out/libation" -type f -name 'CronJob-libation-reconcile.yaml' -print -quit)"

test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

nix run nixpkgs#yq-go -- \
  '.spec.jobTemplate.spec.template.spec.initContainers[0].command[3]' \
  "$manifest" >"$test_root/jitter.sh"

mkdir "$test_root/bin"
printf '%s\n' \
  '#!/bin/sh' \
  'value="$(sed -n "1p" "$JITTER_TEST_SEQUENCE")"' \
  'sed -i "1d" "$JITTER_TEST_SEQUENCE"' \
  'printf "%s\n" "$value"' \
  >"$test_root/bin/od"
printf '%s\n' \
  '#!/bin/sh' \
  'test "$1" -ge 0' \
  'test "$1" -le 7200' \
  'printf "%s\n" "$1" >"$JITTER_TEST_RESULT"' \
  >"$test_root/bin/sleep"
chmod +x "$test_root/bin/od" "$test_root/bin/sleep"

check_case() {
  input="$1"
  expected="$2"
  result="$test_root/result"
  sequence="$test_root/sequence"
  rm -f "$result"
  printf '%s\n' "${input//,/$'\n'}" >"$sequence"

  PATH="$test_root/bin:$PATH" \
    JITTER_TEST_SEQUENCE="$sequence" \
    JITTER_TEST_RESULT="$result" \
    nix run nixpkgs#busybox -- sh "$test_root/jitter.sh" >/dev/null

  actual="$(<"$result")"
  test "$actual" -eq "$expected"
}

check_case "          0" 0
check_case "       7200" 7200
check_case "       7201" 0
check_case "4294964440,4294964439" 7200

echo "libation jitter parsing: representative delays are within [0, 7200]"

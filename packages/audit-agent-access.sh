# Audit that the AI-agent sandbox user is confined as designed. Read-only; run on
# globalhawk (needs sudo to inspect /data and drop to the agent). Reports findings
# and exits non-zero if any check fails. NOT wired into `nix flake check`.

user="${1:-agent}"
proxy="${2:-http://127.0.0.1:2375}"
fail=0

note() { printf '  %s\n' "$1"; }

echo "== group hygiene for '${user}' =="
if ! id "${user}" >/dev/null 2>&1; then
  note "user '${user}' does not exist here (not globalhawk?) — nothing to audit"
  exit 0
fi
mapfile -t groups < <(id -nG "${user}" | tr ' ' '\n')
for bad in wheel docker _media; do
  for g in "${groups[@]}"; do
    if [ "${g}" = "${bad}" ]; then
      note "FAIL: '${user}' is in forbidden group '${bad}'"
      fail=1
    fi
  done
done
if [ "${fail}" -eq 0 ]; then
  note "ok: groups = ${groups[*]}"
fi

echo "== /data has no world-writable paths =="
if [ ! -d /data ]; then
  note "FAIL: /data does not exist or is not mounted — cannot audit it"
  fail=1
else
  writable=$(find /data -xdev -perm -0002 2>/dev/null || true)
  if [ -n "${writable}" ]; then
    note "FAIL: world-writable paths under /data (agent could write these):"
    printf '%s\n' "${writable}" | while IFS= read -r p; do note "    ${p}"; done
    fail=1
  else
    note "ok: no world-writable paths under /data"
  fi
fi

echo "== real docker socket is not accessible to '${user}' =="
if ! sudo -n -u "${user}" true 2>/dev/null; then
  note "FAIL: cannot sudo to '${user}' to run this check — run as the operator/root on globalhawk"
  fail=1
elif sudo -n -u "${user}" test -r /var/run/docker.sock 2>/dev/null; then
  note "FAIL: '${user}' can read the real /var/run/docker.sock"
  fail=1
else
  note "ok: '${user}' cannot read the real docker socket"
fi

echo "== docker-socket-proxy is read-only =="
get_code=$(curl -s -o /dev/null -w '%{http_code}' "${proxy}/version" || echo 000)
post_code=$(curl -s -o /dev/null -w '%{http_code}' -X POST "${proxy}/containers/create" || echo 000)
if [ "${get_code}" = "200" ]; then
  note "ok: GET /version -> 200"
else
  note "FAIL: GET /version -> ${get_code} (proxy up?)"
  fail=1
fi
if [ "${post_code}" = "403" ]; then
  note "ok: POST /containers/create -> 403"
else
  note "FAIL: POST /containers/create -> ${post_code} (expected 403)"
  fail=1
fi

echo
if [ "${fail}" -eq 0 ]; then
  echo "PASS: sandbox user is confined as designed."
else
  echo "FAIL: one or more checks failed (see above)."
fi
exit "${fail}"

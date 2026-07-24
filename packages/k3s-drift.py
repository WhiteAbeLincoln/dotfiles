#!/usr/bin/env python3
"""k3s-drift — read-only reconciliation check for globalhawk's nixidy lane.

`services.k3s.manifests` prunes resources removed from the *contents* of the
single combined nixidy manifest (proven: adding then removing a workload and
running `switch` creates then deletes it, no manual kubectl). So this tool is
NOT a delete mechanism — the cluster already reconciles on `nixos-rebuild
switch`. It is a trust-but-verify report that surfaces the cases k3s's
auto-prune cannot cover:

  * orphans   — resources still owned by the `nixidy` Addon but no longer in the
                desired set. Should be empty; a non-empty list means a prune
                lagged or the whole Addon was dropped (removing the
                `nixidy.source` entry / `manifests.nixidy.enable = false`).
  * missing   — desired resources not yet live (apply error or still settling).
  * untracked — resources hand-created (e.g. `kubectl apply` while debugging) in
                a namespace nixidy owns, that never made it back into Nix.

It only ever reads. Exit 0 = in sync, 1 = drift found, 2 = tool error.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from dataclasses import dataclass, field

# The flake output that machine/globalhawk/k3s.nix delivers via
# services.k3s.manifests.nixidy.source.
FLAKE_ATTR = ".#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage"

# k3s writes the admin kubeconfig here, root-readable only. Plain kubectl (unlike
# `k3s kubectl`) doesn't auto-detect it, so we point KUBECONFIG at it when run as
# root/sudo and nothing else is configured — otherwise kubectl defaults to
# localhost:8080 and every query fails.
K3S_ADMIN_KUBECONFIG = "/etc/rancher/k3s/k3s.yaml"

# The wrangler/objectset annotation the k3s deploy controller stamps on every
# object it applies for a given manifest Addon. All of our workloads share the
# single `nixidy` Addon.
OWNER_ANNOTATION = "objectset.rio.cattle.io/owner-name"
OWNER = "nixidy"

# sops-nix delivers a few Secrets through the SAME k3s auto-deploy dir as a
# parallel Nix authoring lane (machine/globalhawk/sops.nix renders them into
# /var/lib/rancher/k3s/server/manifests/sops-*.yaml). k3s applies each as its
# own Addon, stamping the objectset owner-name with the manifest filename
# (e.g. "sops-mullvad-wg"). These are Nix-authored but never appear in the
# nixidy render this tool diffs against, so the untracked scan must treat them
# as authored, not hand-created. Their apply health is covered by their
# consumers (cert-manager issuance, the gluetun VPN), not by this tool.
SOPS_OWNER_PREFIX = "sops-"

# Kinds nixidy delivers now or has delivered before. The orphan scan queries the
# union of this and whatever kinds appear in the current desired set, so a kind
# that was fully removed (its last object deleted) is still checked for orphans.
CURATED_KINDS = [
    "Namespace",
    "Deployment",
    "StatefulSet",
    "DaemonSet",
    "Service",
    "Ingress",
    "ConfigMap",
    "Secret",
    "NetworkPolicy",
    "ServiceAccount",
    "PersistentVolumeClaim",
    "CronJob",
    "Job",
    "Certificate",
    "ClusterIssuer",
    "Issuer",
    "SealedSecret",
    "TLSStore",
    "IngressRoute",
    "Middleware",
    "EndpointSlice",
]

# User-facing kinds the untracked scan looks for inside nixidy-owned namespaces.
# Deliberately excludes controller-spawned kinds (ReplicaSet, EndpointSlice,
# Pod, tokens) so a hand-applied Deployment stands out instead of drowning.
TOP_LEVEL_KINDS = [
    "Deployment",
    "StatefulSet",
    "DaemonSet",
    "Service",
    "Ingress",
    "ConfigMap",
    "Secret",
    "NetworkPolicy",
    "CronJob",
    "Job",
    "PersistentVolumeClaim",
    "Certificate",
    "SealedSecret",
    "IngressRoute",
    "Middleware",
]


# cert-manager stamps the Secret it issues with the name of its backing
# Certificate. A TLS secret with this annotation is managed by cert-manager, not
# hand-created — but only while that Certificate still exists.
CM_CERT_ANNOTATION = "cert-manager.io/certificate-name"


@dataclass(frozen=True)
class Ref:
    kind: str
    namespace: str  # "" for cluster-scoped
    name: str
    # Human note (e.g. why an untracked resource was flagged). Excluded from
    # equality/hashing so set diffs on (kind, namespace, name) are unaffected.
    note: str = field(default="", compare=False)

    def __str__(self) -> str:
        where = f"-n {self.namespace} " if self.namespace else ""
        base = f"{self.kind}/{self.name} {where}".rstrip()
        return f"{base}  ({self.note})" if self.note else base


def _use_color() -> bool:
    return sys.stdout.isatty()


def hdr(s: str) -> str:
    return f"\033[1m{s}\033[0m" if _use_color() else s


def warn(s: str) -> None:
    print(f"warning: {s}", file=sys.stderr)


def die(msg: str):
    """Fatal tool error (couldn't run) — distinct from drift, which exits 1."""
    print(msg, file=sys.stderr)
    sys.exit(2)


def run(cmd: list[str], *, check: bool = True) -> subprocess.CompletedProcess:
    return subprocess.run(
        cmd, capture_output=True, text=True, check=check
    )


def resolve_kubeconfig(explicit: str | None) -> str | None:
    """Decide which kubeconfig kubectl should use, returning the path we set (or
    None to leave kubectl's own defaults / an inherited $KUBECONFIG in place)."""
    if explicit:
        return explicit
    if os.environ.get("KUBECONFIG"):
        return None  # respect an explicitly-set env
    if os.access(K3S_ADMIN_KUBECONFIG, os.R_OK):
        return K3S_ADMIN_KUBECONFIG  # running as root/sudo — the common case
    return None


def preflight() -> None:
    """Fail fast with a useful message if kubectl can't reach the cluster, rather
    than letting every query error out and misreport the result as 'in sync'."""
    cp = run(["kubectl", "get", "--raw", "/version"], check=False)
    if cp.returncode == 0:
        return
    err = cp.stderr
    # A forbidden /version means the server IS reachable (just RBAC-limited) —
    # fine to proceed. Connection/config errors are fatal.
    if any(
        m in err
        for m in ("refused", "localhost:8080", "Unable to connect", "no configuration has been provided", "was refused")
    ):
        die(
            "error: cannot reach the cluster with kubectl.\n"
            f"  {err.strip().splitlines()[-1] if err.strip() else ''}\n"
            "The k3s admin kubeconfig is root-only, so run this with sudo:\n"
            "  sudo nix run .#k3s-drift\n"
            f"or point --kubeconfig / $KUBECONFIG at a config (e.g. {K3S_ADMIN_KUBECONFIG})."
        )


def build_manifest_dir(flake_ref: str) -> str:
    """Realise the nixidy environment and return its store path."""
    attr = flake_ref + FLAKE_ATTR[1:] if flake_ref != "." else FLAKE_ATTR
    try:
        cp = run(["nix", "build", "--no-link", "--print-out-paths", attr])
    except subprocess.CalledProcessError as e:
        die(f"error: `nix build {attr}` failed:\n{e.stderr.strip()}")
    path = cp.stdout.strip().splitlines()[-1].strip()
    if not path:
        die("error: nix build produced no output path")
    return path


def load_desired(manifest_dir: str) -> set[Ref]:
    """Parse every rendered doc except those under an apps/ path segment.

    Mirrors the `find -L ... -not -path '*/apps/*'` filter in
    machine/globalhawk/k3s.nix — keep the two in sync. apps/ holds ArgoCD
    Application CRs that k3s never receives.
    """
    import yaml

    refs: set[Ref] = set()
    for root, _dirs, files in os.walk(manifest_dir, followlinks=True):
        if f"{os.sep}apps{os.sep}" in f"{os.sep}{os.path.relpath(root, manifest_dir)}{os.sep}":
            continue
        for fn in files:
            if not fn.endswith((".yaml", ".yml")):
                continue
            with open(os.path.join(root, fn)) as fh:
                for doc in yaml.safe_load_all(fh):
                    if not isinstance(doc, dict):
                        continue
                    kind = doc.get("kind")
                    meta = doc.get("metadata") or {}
                    name = meta.get("name")
                    if not kind or not name:
                        continue
                    refs.add(Ref(kind, meta.get("namespace") or "", name))
    return refs


# Kinds a query could not actually read (RBAC forbidden, transport error). A
# desired resource of such a kind must NOT be reported as missing — we simply
# don't know its state — so main() subtracts these out and lists them instead.
UNREADABLE_KINDS: set[str] = set()


def kubectl_get(kind: str, *, namespace: str | None = None) -> list[dict] | None:
    """List a kind as JSON items.

    Returns [] for a genuinely-absent kind (CRD not installed → it has zero
    instances, which is real information). Returns None when the kind could not
    be read at all (forbidden / error); the kind is recorded in UNREADABLE_KINDS
    so the caller can distinguish "not there" from "couldn't look"."""
    base = ["kubectl", "get", kind, "-o", "json"]
    scopes = [["-n", namespace]] if namespace else [["-A"], []]
    last_err = ""
    for scope in scopes:
        cp = run(base + scope, check=False)
        if cp.returncode == 0:
            try:
                return json.loads(cp.stdout).get("items", [])
            except json.JSONDecodeError:
                return []
        last_err = cp.stderr.strip()
        # A cluster-scoped kind rejects -A; retry without it (next scope).
        if "cannot be retrieved by name across all namespaces" in last_err:
            continue
        break
    if any(s in last_err for s in ("doesn't have a resource type", "the server could not find")):
        return []  # CRD not installed — a real, known zero.
    UNREADABLE_KINDS.add(kind)
    if "forbidden" in last_err.lower():
        warn(f"cannot read {kind} (forbidden) — run with admin kubeconfig for full coverage")
    else:
        warn(f"kubectl get {kind} failed: {last_err}")
    return None


def owner_of(item: dict) -> str | None:
    return (item.get("metadata", {}).get("annotations") or {}).get(OWNER_ANNOTATION)


def has_controller(item: dict) -> bool:
    for ref in item.get("metadata", {}).get("ownerReferences", []) or []:
        if ref.get("controller"):
            return True
    return False


def load_live_owned(query_kinds: set[str]) -> set[Ref]:
    """Every live resource stamped with our objectset owner annotation."""
    owned: set[Ref] = set()
    for kind in sorted(query_kinds):
        items = kubectl_get(kind)
        if not items:
            continue
        for it in items:
            if owner_of(it) == OWNER:
                meta = it.get("metadata", {})
                owned.add(Ref(kind, meta.get("namespace") or "", meta["name"]))
    return owned


# Auto-created objects that live in every namespace and are never authored in
# Nix — excluded from the untracked scan so they don't read as drift.
def _is_boilerplate(kind: str, item: dict) -> bool:
    meta = item.get("metadata", {})
    name = meta.get("name", "")
    if kind == "ConfigMap" and name == "kube-root-ca.crt":
        return True
    if kind == "ServiceAccount" and name == "default":
        return True
    if kind == "Secret" and item.get("type", "").startswith("kubernetes.io/service-account-token"):
        return True
    if kind == "Secret" and item.get("type") == "helm.sh/release.v1":
        return True
    return False


def load_live_certificates() -> set[tuple[str, str]]:
    """(namespace, name) of every live cert-manager Certificate, used to tell an
    actively-managed TLS secret from an orphaned one whose Certificate is gone."""
    items = kubectl_get("Certificate")
    if not items:
        return set()
    return {(it["metadata"].get("namespace", ""), it["metadata"]["name"]) for it in items}


def load_untracked(nixidy_namespaces: set[str], live_certs: set[tuple[str, str]]) -> list[Ref]:
    """Top-level resources in nixidy-owned namespaces that are neither
    nixidy-owned nor controller-spawned — i.e. hand-created and un-captured.

    cert-manager TLS secrets get no ownerReference by default, so they'd look
    hand-created. Classify them precisely: skip while their Certificate exists
    (actively managed on behalf of an Ingress), flag once it's gone (a true
    orphan cert-manager leaves behind — e.g. the old per-app *-tls secrets)."""
    found: list[Ref] = []
    for ns in sorted(nixidy_namespaces):
        for kind in TOP_LEVEL_KINDS:
            items = kubectl_get(kind, namespace=ns)
            if not items:
                continue
            for it in items:
                owner = owner_of(it)
                if owner == OWNER or has_controller(it) or _is_boilerplate(kind, it):
                    continue
                # Authored via sops-nix through the k3s manifests dir (see
                # SOPS_OWNER_PREFIX) — Nix-owned, just not in the nixidy render.
                if owner and owner.startswith(SOPS_OWNER_PREFIX):
                    continue
                meta = it.get("metadata", {})
                name = meta.get("name", "")
                cert = (meta.get("annotations") or {}).get(CM_CERT_ANNOTATION)
                if kind == "Secret" and cert is not None:
                    if (ns, cert) in live_certs:
                        continue  # active cert-manager secret — not drift
                    found.append(Ref(kind, ns, name, note=f"orphaned cert-manager secret; Certificate/{cert} is gone"))
                    continue
                found.append(Ref(kind, ns, name))
    return found


def report(title: str, refs, *, hint: str = "") -> None:
    print(hdr(title))
    if not refs:
        print("  (none)")
    else:
        for r in sorted(refs, key=lambda x: (x.kind, x.namespace, x.name)):
            print(f"  {r}")
        if hint:
            print(f"  → {hint}")
    print()


def main() -> int:
    ap = argparse.ArgumentParser(description="Read-only drift check between the nixidy desired state and live k3s.")
    ap.add_argument("--manifest-dir", help="Use an already-rendered manifest tree instead of building the flake.")
    ap.add_argument("--flake", default=".", help="Flake ref to build the nixidy env from (default: current dir).")
    ap.add_argument("--kubeconfig", help=f"kubeconfig to use (default: $KUBECONFIG, else {K3S_ADMIN_KUBECONFIG} if readable).")
    args = ap.parse_args()

    kubeconfig = resolve_kubeconfig(args.kubeconfig)
    if kubeconfig:
        os.environ["KUBECONFIG"] = kubeconfig
    preflight()

    manifest_dir = args.manifest_dir or build_manifest_dir(args.flake)
    desired = load_desired(manifest_dir)
    if not desired:
        die(f"error: no manifests found under {manifest_dir}")

    nixidy_namespaces = {r.name for r in desired if r.kind == "Namespace"}
    live_owned = load_live_owned(set(CURATED_KINDS) | {r.kind for r in desired})

    orphans = live_owned - desired
    # Don't flag a desired resource as missing if we couldn't read its kind —
    # that's unknown, not absent. Report those separately.
    missing = {r for r in desired - live_owned if r.kind not in UNREADABLE_KINDS}
    unverifiable = {r for r in desired if r.kind in UNREADABLE_KINDS}
    untracked = load_untracked(nixidy_namespaces, load_live_certificates())

    print(f"desired: {len(desired)} resources  |  nixidy-owned live: {len(live_owned)}")
    print(f"namespaces owned by nixidy: {', '.join(sorted(nixidy_namespaces)) or '(none)'}\n")

    report(
        "orphans (owned by nixidy, live, not desired):",
        orphans,
        hint="prune lagged or Addon dropped; remove with: sudo k3s kubectl delete <above>",
    )
    report(
        "missing (desired, not live):",
        missing,
        hint="apply error or still settling; check k3s manifest logs, then re-switch",
    )
    report(
        f"untracked (hand-created in {', '.join(sorted(nixidy_namespaces)) or 'nixidy'} namespaces):",
        untracked,
        hint="not authored in Nix; add to k8s/** or delete by hand",
    )
    if unverifiable:
        report(
            f"unverifiable (kinds not readable with this kubeconfig: {', '.join(sorted(UNREADABLE_KINDS))}):",
            unverifiable,
            hint="state unknown, not counted as drift; re-run with admin kubeconfig",
        )

    drift = bool(orphans or missing or untracked)
    print(hdr("DRIFT DETECTED" if drift else "in sync"))
    return 1 if drift else 0


if __name__ == "__main__":
    sys.exit(main())

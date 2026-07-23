# Shared helpers for the globalhawk nixidy modules.
{lib}: rec {
  # Common labels applied to every workload we author, so NetworkPolicy and
  # kubectl selectors have a stable handle.
  appLabels = name: {
    "app.kubernetes.io/name" = name;
    "app.kubernetes.io/managed-by" = "nixidy";
  };
  # mkArrApp is defined in Task 1.2 (Phase 1). Placeholder kept intentionally
  # minimal until then.
}

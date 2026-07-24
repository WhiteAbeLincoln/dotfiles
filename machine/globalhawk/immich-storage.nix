# Host-side identity + storage isolation for Immich (the k8s workloads live in
# k8s/apps/immich.nix). Immich runs as its OWN uid (not the shared _media 994),
# and its data tree is 0750 immich:immich, so the media apps that bind-mount all
# of mediaRoot (radarr/sonarr) are denied it by the kernel — no arr change, and
# hardlinks stay intact. `abe`/`agent` keep read access via the media-readers
# group + a default ACL. See docs/superpowers/specs/2026-07-24-globalhawk-immich-k3s-design.md.
{...}: let
  facts = import ./facts.nix;
  immichRoot = "${facts.mediaRoot}/immich";
in {
  users.groups.immich.gid = facts.immichUid;
  users.users.immich = {
    isSystemUser = true;
    uid = facts.immichUid;
    group = "immich";
    description = "Immich service account (k8s workload uid)";
  };

  # Reusable human-read handle for tightened per-app media trees. `abe` also has
  # _media (write) elsewhere; `agent` (read-only sandbox, uid 1001) is kept OUT
  # of _media and only ever gets read, via this group.
  users.groups.media-readers.members = ["abe" "agent"];

  # Ownership + ACLs. The dir mode (0750) denies `_media` (994) — it's neither
  # owner nor in the immich/media-readers groups, so it falls to "other" = ---.
  # media-readers gets r-x on the tree root (traverse) and on library/ (read
  # photos), plus a default ACL on library/ so photos Immich creates inherit it.
  # pgdata/model-cache stay human-inaccessible (no reader ACL) — nobody browses
  # Postgres files. `A+` (append) per the ebook-stack overlapping-ACL lesson.
  systemd.tmpfiles.rules = [
    "d ${immichRoot} 0750 immich immich - -"
    "d ${immichRoot}/library 0750 immich immich - -"
    "d ${immichRoot}/pgdata 0750 immich immich - -"
    "d ${immichRoot}/model-cache 0750 immich immich - -"
    "A+ ${immichRoot} - - - - group:media-readers:r-x,mask::r-x"
    "A+ ${immichRoot}/library - - - - group:media-readers:r-x,default:group:media-readers:r-x,mask::r-x,default:mask::r-x"
  ];
}

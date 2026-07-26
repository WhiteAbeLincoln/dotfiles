# Host-side identity + storage isolation for Immich (the k8s workloads live in
# k8s/apps/immich.nix). Immich runs as its OWN uid (not the shared _media 994),
# and its data tree is 0750 immich:immich, so the media apps that bind-mount all
# of mediaRoot (radarr/sonarr) are denied it by the kernel — no arr change, and
# hardlinks stay intact. `abe`/`agent` keep read access via the media-readers
# group + a default ACL. See docs/superpowers/specs/2026-07-24-globalhawk-immich-k3s-design.md.
{
  config,
  pkgs,
  ...
}: let
  mediaRoot = config.homelab.media.root;
  immichRoot = "${mediaRoot}/immich";
in {
  users.groups.immich.gid = config.users.users.immich.uid;
  users.users.immich = {
    isSystemUser = true;
    uid = 988;
    group = "immich";
    description = "Immich service account (k8s workload uid)";
  };

  # Reusable human-read handle for tightened per-app media trees. `abe` also has
  # _media (write) elsewhere; `agent` (read-only sandbox, uid 1001) is kept OUT
  # of _media and only ever gets read, via this group.
  users.groups.media-readers.members = ["abe" "agent"];

  # Create + own + ACL the Immich data tree with a oneshot, NOT
  # systemd.tmpfiles. tmpfiles refuses to operate through the ownership
  # transition /data/Media (_media) -> /data/Media/immich (immich) — it logs
  # "Detected unsafe path transition" and exits 73, silently failing to create
  # the subdirs (which then breaks the hostPath `type: Directory` mounts). Plain
  # install/setfacl as root have no such safety check. Idempotent, so it also
  # re-asserts ownership/ACLs on every boot/switch.
  #
  # 0750 + owner immich denies the shared `_media` (994) apps (radarr/sonarr
  # bind-mount all of mediaRoot) — 994 is "other" here. The media-readers ACL
  # (default on library/, so it's inherited umask-proof) gives abe/agent read.
  # pgdata/model-cache get no reader ACL — nobody browses Postgres/model files.
  # hostPath pods need these dirs to pre-exist; strict ordering isn't required
  # (kubelet retries FailedMount), only eventual creation.
  systemd.services.immich-storage-dirs = {
    description = "Create + own + ACL the Immich data tree (tmpfiles can't cross the _media->immich owner transition)";
    wantedBy = ["local-fs.target"];
    after = ["zfs-media-posixacl.service"];
    unitConfig.RequiresMountsFor = mediaRoot;
    path = [pkgs.coreutils pkgs.acl];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      install -d -o immich -g immich -m 0750 ${immichRoot}
      install -d -o immich -g immich -m 0750 ${immichRoot}/library ${immichRoot}/pgdata ${immichRoot}/model-cache
      setfacl -m g:media-readers:r-x,m::r-x ${immichRoot}
      setfacl -m g:media-readers:r-x,d:g:media-readers:r-x,m::r-x,d:m::r-x ${immichRoot}/library
    '';
  };
}

# Work in Progress Spec: Kubernetes Storage Modernization

The current workloads use two distinct kinds of `hostPath`, and they need different fixes.

## Strong PVC candidates

These are application-owned state with a single writer. They should become PVCs provisioned by a ZFS-aware CSI driver:

- Authelia SQLite data at `/var/lib/authelia`
- Immich PostgreSQL data
- Audiobookshelf config and metadata
- Libation config and database
- Calibre Web Automated config
- qBittorrent config
- Prowlarr, Radarr, and Sonarr config directories created through `mkLsioApp`

The recommended eventual foundation is OpenEBS ZFS LocalPV backed by a dedicated child dataset of `pool/media`. Each PVC would receive its own ZFS dataset, size policy, snapshot boundary, expansion support, and Kubernetes lifecycle. OpenEBS supports dynamically provisioning datasets with ZFS properties through a StorageClass. [OpenEBS ZFS LocalPV](https://openebs.io/docs/main/user-guides/local-storage-user-guide/local-pv-zfs/configuration/zfs-usage)

This improves ownership, scheduling, declarative lifecycle, snapshots, and backup integration. It does not create high availability: the volumes remain tied to `globalhawk` until another storage node or network storage exists.

## Existing shared data

These directories are deliberately shared among workloads or directly managed outside Kubernetes:

- Movies, TV, music, books, anime, and audiobook libraries
- Torrent downloads
- Immich’s photo library
- Calibre ingest and Libation in-progress exchange directories

They should not each be copied into dynamically provisioned private PVCs. The better Kubernetes abstraction is one or more statically defined `local` PersistentVolumes backed by the existing ZFS datasets/directories, claimed by the relevant workloads. Unlike raw `hostPath`, local PVs express node affinity and let the scheduler understand the storage constraint. [Kubernetes local volumes](https://kubernetes.io/docs/concepts/storage/volumes/)

Longer term, these could become CSI-managed shared datasets or NFS-backed `ReadWriteMany` volumes if the cluster gains nodes. On today’s single node, static local PVs preserve the existing sharing and performance model.

## Ephemeral data

- Immich’s machine-learning model cache can become an `emptyDir` with a size limit if re-download time is acceptable, or a low-priority PVC if retaining the cache matters.
- Immich Valkey is already effectively ephemeral and needs no PVC.
- `/dev/net/tun` is a device mount, not persistent application storage, so its `hostPath` remains appropriate.

## Suggested migration order

1. Install and validate a ZFS CSI StorageClass.
2. Migrate a low-risk config workload first.
3. Migrate Authelia and other SQLite/config workloads with backup-and-restore checks.
4. Migrate Immich PostgreSQL only after snapshot, restore, and rollback procedures are proven.
5. Represent shared media paths as static local PVs without moving their data.
6. Update `mkLsioApp` to accept claims, making raw config `hostPath` the exceptional path.

This work is explicitly out of scope for the observability implementation, including the new monitoring PVCs; they will use the existing k3s local-path provisioner for now.

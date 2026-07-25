# Host-side identity + a persistent dir for Authelia's SQLite storage (the k8s
# workload lives in k8s/apps/authelia.nix). Authelia runs as its OWN uid and its
# state dir is 0750 authelia:authelia. The pod mounts /var/lib/authelia via a
# hostPath patched onto the Helm-rendered Deployment.
{...}: let
  facts = import ./facts.nix;
  uid = facts.autheliaUid;
in {
  users.users.authelia = {
    isSystemUser = true;
    group = "authelia";
    uid = uid;
    description = "Authelia SSO";
  };
  users.groups.authelia.gid = uid;

  systemd.tmpfiles.rules = [
    "d /var/lib/authelia 0750 authelia authelia - -"
  ];
}

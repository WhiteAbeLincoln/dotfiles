# Publish every k3s ingress host as an mDNS **CNAME -> globalhawk.local** so the
# names resolve on the LAN — including macOS/iOS Bonjour, which follows mDNS
# CNAMEs (confirmed via `dns-sd`). CNAMEs (not address records) mean resolution
# is delegated to avahi's own host publishing: dual-stack (v4+v6) and resilient
# to the DHCP IP changing, with no address logic here.
#
# Uses go-avahi-cname (flake input) rather than an ad-hoc avahi-publish loop. The
# alias list is DERIVED from the nixidy env's Ingress hosts — add a k8s app with
# an ingress and its alias publishes automatically.
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  goAvahiCname = inputs.go-avahi-cname.packages.${pkgs.stdenv.hostPlatform.system}.default;

  env = inputs.self.nixidyEnvs.x86_64-linux.globalhawk;
  allObjects = lib.concatMap (app: app.objects) (lib.attrValues env.config.applications);
  ingressHosts =
    lib.concatMap (
      o:
        if (o.kind or "") == "Ingress"
        then lib.concatMap (rule: lib.optional (rule ? host) rule.host) (o.spec.rules or [])
        else []
    )
    allObjects;

  # Trailing dot = publish the name verbatim as a CNAME. Without it, go-avahi-cname
  # treats a bare name as a SUBDOMAIN of the host (e.g. radarr-globalhawk.local ->
  # radarr-globalhawk.globalhawk.local), which is a 3-label name macOS won't
  # resolve over mDNS.
  aliasFqdns = map (h: "${h}.") (lib.unique ingressHosts);
  hostFqdn = "${config.networking.hostName}.local.";
in {
  systemd.services.mdns-aliases = {
    description = "Publish k3s ingress hostnames as mDNS CNAMEs -> ${hostFqdn}";
    after = ["avahi-daemon.service" "network-online.target"];
    wants = ["avahi-daemon.service" "network-online.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = "${goAvahiCname}/bin/go-avahi-cname cname --fqdn ${hostFqdn} ${lib.escapeShellArgs aliasFqdns}";
      Restart = "on-failure";
      RestartSec = 5;
      # Unprivileged: it only needs the system D-Bus to reach avahi, which its
      # default policy permits for any user.
      DynamicUser = true;
    };
  };
}

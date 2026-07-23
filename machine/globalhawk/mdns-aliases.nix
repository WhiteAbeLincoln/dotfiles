# Publish mDNS aliases so every k8s ingress host resolves on the LAN (phones,
# laptops, avahi-resolve). avahi only advertises the host's own name
# (globalhawk.local); each ingress hostname is a *separate* single-label .local
# name that must be published explicitly.
#
# The alias list is DERIVED from the nixidy env's Ingress resources — add an app
# with an ingress in k8s/ and its alias is published automatically, no edit here.
# avahi-publish only does address records (no CNAME), so we publish A records at
# this host's current IPv4, re-resolved on every (re)start.
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  env = inputs.self.nixidyEnvs.x86_64-linux.globalhawk;
  # Every rendered k8s object across all applications.
  allObjects = lib.concatMap (app: app.objects) (lib.attrValues env.config.applications);
  # Hosts from every Ingress rule.
  ingressHosts =
    lib.concatMap (
      o:
        if (o.kind or "") == "Ingress"
        then lib.concatMap (rule: lib.optional (rule ? host) rule.host) (o.spec.rules or [])
        else []
    )
    allObjects;
  aliases = lib.unique ingressHosts;
in {
  systemd.services.avahi-aliases = {
    description = "Publish k8s ingress mDNS aliases pointing at this host";
    after = ["avahi-daemon.service" "network-online.target"];
    wants = ["avahi-daemon.service" "network-online.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
    };
    script = ''
      set -eu
      ip=$(${pkgs.iproute2}/bin/ip -4 route get 1.1.1.1 | ${pkgs.gnugrep}/bin/grep -oP 'src \K[0-9.]+')
      echo "publishing ${toString (lib.length aliases)} aliases -> $ip"
      pids=""
      for a in ${lib.concatStringsSep " " aliases}; do
        ${pkgs.avahi}/bin/avahi-publish -a -R "$a" "$ip" &
        pids="$pids $!"
      done
      # If any single publisher exits (e.g. daemon restart), tear the rest down
      # so systemd restarts the whole set and re-resolves the IP.
      wait -n
      kill $pids 2>/dev/null || true
      exit 1
    '';
  };
}

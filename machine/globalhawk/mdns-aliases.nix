# Publish mDNS aliases so <app>-globalhawk.local resolves on the LAN (phones,
# laptops, avahi-resolve). avahi only advertises the host's own name
# (globalhawk.local); each ingress hostname is a *separate* single-label .local
# name that must be published explicitly. avahi-publish only does address
# records (no CNAME), so we publish A records at this host's current IPv4,
# re-resolved on every (re)start. Keep `apps` in sync with the k8s ingresses.
{
  config,
  pkgs,
  lib,
  ...
}: let
  ingressSuffix = (import ../../secrets/globalhawk.nix).ingressSuffix;
  apps = [
    "whoami"
    "plex"
    "prowlarr"
    "radarr"
    "sonarr"
    "qbittorrent"
  ];
  aliases = map (a: a + ingressSuffix) apps;
in {
  systemd.services.avahi-aliases = {
    description = "Publish <app>${ingressSuffix} mDNS aliases pointing at this host";
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

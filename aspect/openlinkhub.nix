{...}: {
  nixos = {pkgs, ...}: {
    users.groups.openlinkhub = {};
    users.users.openlinkhub = {
      isSystemUser = true;
      group = "openlinkhub";
    };

    services.udev.packages = [pkgs.openlinkhub];
    systemd.services.openlinkhub = {
      description = "Open source interface for Corsair iCUE LINK hubs";
      wantedBy = ["multi-user.target"];
      after = ["systemd-udevd.service"];
      preStart = ''
        install -d database
        cp --recursive --no-clobber ${pkgs.openlinkhub}/opt/OpenLinkHub/database/. database/
        chmod --recursive u+w database
        ln --symbolic --force --no-dereference ${pkgs.openlinkhub}/opt/OpenLinkHub/static static
        ln --symbolic --force --no-dereference ${pkgs.openlinkhub}/opt/OpenLinkHub/web web
      '';
      serviceConfig = {
        User = "openlinkhub";
        Group = "openlinkhub";
        WorkingDirectory = "/var/lib/openlinkhub";
        StateDirectory = "openlinkhub";
        ExecStartPre = [
          "+${pkgs.systemd}/bin/udevadm trigger --subsystem-match=usb --attr-match=idVendor=1b1c"
          "+${pkgs.systemd}/bin/udevadm settle --timeout=30"
        ];
        ExecStart = "${pkgs.openlinkhub}/bin/OpenLinkHub";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}

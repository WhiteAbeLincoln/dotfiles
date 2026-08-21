{
  nixos = {
    config,
    lib,
    pkgs,
    ...
  }: {
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      publish = {
        enable = true;
        addresses = true;
      };
      extraServiceFiles = lib.mkMerge [
        (lib.mkIf config.services.openssh.enable {
          ssh = "${pkgs.avahi}/etc/avahi/services/ssh.service";
          sftp-ssh = "${pkgs.avahi}/etc/avahi/services/sftp-ssh.service";
        })
        (lib.mkIf config.services.samba.enable {
          smb = ''
            <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
            <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
            <service-group>
              <name replace-wildcards="yes">%h</name>
              <service>
                <type>_smb._tcp</type>
                <port>445</port>
              </service>
            </service-group>
          '';
        })
      ];
    };
  };
}

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.wireguard;
in {
  options.modules.services.wireguard = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wireguard-tools ];

    networking.firewall = {
      allowedUDPPorts = [ 51820 ];

      # if packets are still dropped, they will show up in dmesg
      logReversePathDrops = true;
    };

    networking.wg-quick.interfaces = {
      wg0 = {
        address = [ "10.64.173.1/32" "fc00:bbbb:bbbb:bb01::1:ad00/128" ];
        dns = [ "10.64.0.1" ];
        privateKeyFile = config.age.secrets.mullvad.path;
        peers = [
          {
            publicKey = "TNrdH73p6h2EfeXxUiLOCOWHcjmjoslLxZptZpIPQXU=";
            allowedIPs = [ "0.0.0.0/0" "::0/0" ];
            endpoint = "146.70.116.98:51820";
            persistentKeepalive = 25;
          }
        ];
      };
    };
  };
}

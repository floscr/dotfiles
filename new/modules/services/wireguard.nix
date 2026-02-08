{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.wireguard;
in {
  options.modules.services.wireguard = {
    enable = mkBoolOpt false;
    mullvad.enable = mkBoolOpt false;
    hetzner.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wireguard-tools ];

    networking.firewall = {
      allowedUDPPorts = [ 51820 ];
      logReversePathDrops = true;
    };

    networking.wg-quick.interfaces = mkMerge [
      (mkIf cfg.mullvad.enable {
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
      })

      (mkIf cfg.hetzner.enable {
        wg-hetzner = {
          address = [ "10.100.0.3/24" ];
          dns = [ "1.1.1.1" ];
          privateKeyFile = config.age.secrets.wg-hetzner.path;
          peers = [
            {
              publicKey = "HALTk5Isj/Ji7sn3Ac2JzLBlM7/LIqqhbpdpFYbhRDI=";
              allowedIPs = [ "10.100.0.0/24" ];
              endpoint = "46.224.184.176:51820";
              persistentKeepalive = 25;
            }
          ];
        };
      })
    ];
  };
}

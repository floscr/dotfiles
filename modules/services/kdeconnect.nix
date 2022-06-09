{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.kdeconnect;
in
{
  options.modules.services.kdeconnect = {
    enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [
      kdeconnect
    ];

    networking.firewall.allowedTCPPortRanges = [{
      from = 1714;
      to = 1764;
    }];

    home-manager.users.${config.user.name}.services.kdeconnect.enable = true;
  };
}

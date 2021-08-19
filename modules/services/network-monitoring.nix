{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.network-monitoring;
in
{
  options.modules.services.network-monitoring = {
    enable = mkBoolOpt false;
    interface = mkStrOpt "wlp0s20f3";
  };

  config = {
    user.packages = with pkgs; [
      (
        let
          vnstat = "${pkgs.vnstat}/bin/vnstat";
        in
        pkgs.writeScriptBin "network-monitor-status" ''
          #!${stdenv.shell}
          # Get the total rate of the downloaded data
          ${vnstat} -i ${cfg.interface} | grep -m 1 "total" | awk '{print $8" "substr ($9, 1, 1)}'
        ''
      )
    ];
    systemd.services.vnstat = {
      # Dont autostart service
      after = [ ];
      wantedBy = [ ];
    };
    services = {
      vnstat.enable = true;
    };
  };
}

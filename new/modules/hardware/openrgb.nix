{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.openrgb;
in
{
  options.modules.hardware.openrgb = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.hardware.openrgb = {
      enable = true;
      motherboard = "amd";
    };

    # Turn off all LEDs at boot and shutdown
    systemd.services.openrgb-off = {
      description = "Turn off all RGB LEDs";
      after = [ "openrgb.service" ];
      wants = [ "openrgb.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.openrgb}/bin/openrgb --mode static --color 000000";
        ExecStop = "${pkgs.openrgb}/bin/openrgb --mode static --color 000000";
      };
    };

    user.packages = [ pkgs.openrgb ];
  };
}

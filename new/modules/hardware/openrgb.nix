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
      environment.QT_QPA_PLATFORM = "offscreen";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 5";
        ExecStart = "${pkgs.openrgb}/bin/openrgb --client --mode off";
        ExecStop = "${pkgs.openrgb}/bin/openrgb --client --mode off";
      };
    };

    user.packages = [ pkgs.openrgb ];
  };
}

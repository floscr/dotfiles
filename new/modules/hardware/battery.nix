{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.battery;
in
{
  options.modules.hardware.battery = {
    enable = mkBoolOpt false;
    id = mkStrOpt "ACPI0003:00 00000080 00000001";
  };

  config = mkIf cfg.enable {
    services.upower.enable = true;
    services.tlp = {
      enable = true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 92;
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_BOOST_ON_AC = 1;
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        ENERGY_PERF_POLICY_ON_BAT = "powersave";
      };
    };
    services.acpid = {
      enable = true;
      handlers = {
        acConnect = {
          event = "ac_adapter ${cfg.id}";
          action = ''
            echo "AC Adapter connected"
            ${pkgs.dunst}/bin/dunstify -C $(cat /tmp/battery_notification_id)
          '';
        };
      };
    };
  };
}

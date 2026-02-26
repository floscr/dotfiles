{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.hardware.battery;
  stateDir = "/tmp/battery_state";

  battery-notify = pkgs.writeShellScriptBin "battery-notify" ''
    STATE_DIR="${stateDir}"
    NOTIFIED_FILE="$STATE_DIR/notified"
    ID_FILE="$STATE_DIR/notification_id"

    mkdir -p "$STATE_DIR"

    # Don't show if already notified in this power cycle
    if [ -f "$NOTIFIED_FILE" ]; then
      exit 0
    fi

    # Show notification and save ID
    id=$(${pkgs.dunst}/bin/dunstify -p -u critical -a Battery "Battery Low" "Your computer will turn off soon")
    echo "$id" > "$ID_FILE"
    touch "$NOTIFIED_FILE"
  '';
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

    user.packages = [ battery-notify ];

    services.acpid = {
      enable = true;
      handlers = {
        acConnect = {
          event = "ac_adapter ${cfg.id}";
          action = ''
            echo "AC Adapter connected"
            # Reset notification state so it can fire again on next unplug
            rm -f ${stateDir}/notified

            # Close existing notification
            id_file="${stateDir}/notification_id"
            if [ -f "$id_file" ] && [ -s "$id_file" ]; then
              uid=$(${pkgs.coreutils}/bin/id -u ${config.user.name})
              ${pkgs.sudo}/bin/sudo -u ${config.user.name} \
                env DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$uid/bus" \
                ${pkgs.dunst}/bin/dunstify -C $(cat "$id_file")
              rm -f "$id_file"
            fi
          '';
        };
      };
    };
  };
}

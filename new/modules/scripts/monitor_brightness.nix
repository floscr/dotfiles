{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.monitor_brightness;
in
{
  options.modules-new.scripts.monitor_brightness = with my; {
    enable = mkBoolOpt false;
  };

  config =
    let
      ddcutil = "${pkgs.ddcutil}/bin/ddcutil";
      monitor_brightness = (pkgs.writeBb "monitor_brightness" {
        content = ./src/monitor_brightness.clj;
        deps = with pkgs; [
          dunst
        ];
      });
      cmd = "${monitor_brightness}/bin/monitor_brightness";
    in
    {
      # Set the correct permissions to use ddcutil without sudo
      hardware.i2c.enable = true;

      user.packages = with pkgs;
        [
          monitor_brightness
        ];

      modules.bindings.items = [
        {
          command = "${cmd} toggle";
          description = "Toggle Monitor Brightness";
        }
        {
          command = "${cmd} rofi";
          description = "Monitor Brightness (rofi)";
        }
        {
          command = "${cmd} max";
          description = "Max Monitor Brightness (External)";
        }
      ];
    };
}

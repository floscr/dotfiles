{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.invert_colors;
in
{
  options.modules-new.scripts.invert_colors = with my; {
    enable = mkBoolOpt false;
  };

  config =
    let
      monitor_brightness = (pkgs.writeBb "monitor_brightness" {
        content = ./src/monitor_brightness.clj;
      });
    in
    {
      user.packages = with pkgs; [
        monitor_brightness
      ];
      modules.bindings.items = [
        {
          command = "${monitor_brightness}/bin/monitor_brightness toggle";
          description = "Toggle Monitor Brightness";
        }
        {
          command = "${monitor_brightness}/bin/monitor_brightness max";
          description = "Max Monitor Brightness";
        }
      ];
    };
}

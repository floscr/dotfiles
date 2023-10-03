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
      monitor_brightness = (pkgs.writeBb "monitor_brightness" {
        content = ./src/monitor_brightness.clj;
      });
    in
    {
      user.packages = with pkgs; [
        monitor_brightness
      ];

      security.sudo.extraRules = [{
        commands = [
          {
            command = "${pkgs.ddcutil}/bin/ddcutil";
            options = [ "NOPASSWD" ];
          }
        ];
        groups = [ "wheel" ];
      }];

      modules.bindings.items = let cmd = "${monitor_brightness}/bin/monitor_brightness"; in [
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

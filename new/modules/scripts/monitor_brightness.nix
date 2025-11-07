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
        content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/monitor_brightness.clj";
        useSourcePath = true;
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
          ddcutil
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
        {
          xmonadBinding = "M-o 1";
          command = "${cmd} set 10";
        }
        {
          xmonadBinding = "M-o 2";
          command = "${cmd} set 20";
        }
        {
          xmonadBinding = "M-o 3";
          command = "${cmd} set 30";
        }
        {
          xmonadBinding = "M-o 4";
          command = "${cmd} set 40";
        }
        {
          xmonadBinding = "M-o 5";
          command = "${cmd} set 50";
        }
        {
          xmonadBinding = "M-o 6";
          command = "${cmd} set 60";
        }
        {
          xmonadBinding = "M-o 7";
          command = "${cmd} set 70";
        }
        {
          xmonadBinding = "M-o 8";
          command = "${cmd} set 80";
        }
        {
          xmonadBinding = "M-o 9";
          command = "${cmd} set 90";
        }
        {
          xmonadBinding = "M-o 0";
          command = "${cmd} set 100";
        }
      ];
    };
}

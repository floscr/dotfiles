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
      user.packages = with pkgs;
        [
          monitor_brightness
        ];
      modules.bindings.items = [
        {
          xmonadBinding = "M-i";
          command = "${monitor_brightness}/bin/monitor_brightness";
          description = "Invert Colors";
        }
      ];
    };
}

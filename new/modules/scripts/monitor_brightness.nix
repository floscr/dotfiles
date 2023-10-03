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
      hardware.i2c.enable = true;

      # boot.kernelModules = [ "i2c-dev" ];
      # services.udev.extraRules = builtins.readFile
      #   "${pkgs.ddcutil}/share/ddcutil/data/45-ddcutil-i2c.rules";

      # # services.udev.extraRules = ''
      # #   KERNEL=="i2c-12", SUBSYSTEM=="i2c-dev", GROUP=="ddc", MODE="0660"
      # #   KERNEL=="i2c-18", SUBSYSTEM=="i2c-dev", GROUP=="ddc", MODE="0660"
      # # '';
      # user.extraGroups = [ "i2c" ];


      user.packages = with pkgs;
        [
          monitor_brightness
        ];
      # security.sudo = {
      #   extraConfig = ''
      #     %wheel      ALL=(ALL:ALL) NOPASSWD: ${pkgs.ddcutil}/bin/ddcutil
      #   '';
      #   extraRules = [{
      #     commands = [
      #       {
      #         command = "${pkgs.ddcutil}/bin/ddcutil";
      #         options = [ "NOPASSWD" ];
      #       }
      #     ];
      #     users = [ config.user.name ];
      #   }];
      # };

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

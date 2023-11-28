{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.bbluetooth;
in
{
  options.modules-new.scripts.bbluetooth = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "bbluetooth" {
          content = ./src/bbluetooth.clj;
        });
        cmd = "${pkg}/bin/bbluetooth";
      in
      {
        user.packages = with pkgs; [
          pkg
        ];
        modules.bindings.items = [
          {
            description = "Bluetooth: Connect device";
            command = "${cmd} rofi";
          }
          {
            xmonadBinding = "M-o a";
            command = "${cmd} rofi";
          }
          {
            description = "Bluetooth: Off";
            command = "${cmd} off";
          }
          {
            description = "Bluetooth: On";
            command = "${cmd} on";
          }
        ];
      }

    );
}

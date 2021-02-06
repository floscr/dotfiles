{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.external-display;
in {
  options.modules.hardware.external-display = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.ddcutil ];
    boot.kernelModules = [ "i2c_dev" ];
    user.extraGroups = [ "i2c" ];
    services = {
      udev.extraRules = ''
        KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
      '';
    };

    modules.bindings.items = [
      {
        description = "Toggle Monitor Brightness";
        categories = "Script";
        command = "toggle_external_display_brightness";
      }
      {
        description = "Max Monitor Brightness";
        categories = "Script";
        command = "ddcutil setvcp 10 100";
      }
    ];
  };
}

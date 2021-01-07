{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.redshift;
in {
  options.modules.services.redshift = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.redshift = {
      enable = true;
      temperature = {
        day = 5500;
        night = 3000;
      };
    };

    modules.bindings.items = [
      {
        description = "Toggle Redshift";
        categories = "Script";
        command = "toggle_redshift";
      }
    ];
  };
}

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.opensnitch;
in
{
  options.modules.services.opensnitch = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.opensnitch = {
      enable = true;
    };
    home-manager.users.${config.user.name}.services.opensnitch-ui.enable = true;
  };
}

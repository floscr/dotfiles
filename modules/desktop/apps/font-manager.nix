{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.font-manager;
in
{
  options.modules.desktop.apps.font-manager = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      font-manager
    ];
    home.defaultApplications = {
      "font/sfnt" = [ "font-manager.desktop" ];
    };
  };
}

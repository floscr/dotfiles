{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modulesl.desktop.apps.font-manager;
in
{
  options.modules.desktop.apps.font-manager = {
    enable = my.mkBoolOpt false;
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

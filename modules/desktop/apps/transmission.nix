{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.transmission;
in {
  options.modules.desktop.apps.transmission= {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      transmission
    ];
    home-manager.users.${config.user.name}.xdg.mimeApps = {
      associations.added= {
        "x-scheme-handler/magnet" = "transmission-gtk.desktop";
      };
      defaultApplications = {
        "x-scheme-handler/magnet" = "transmission-gtk.desktop";
      };
    };
  };
}

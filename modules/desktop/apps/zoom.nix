{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.desktop.apps.zoom;
in
{
  options.modules.desktop.apps.zoom = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.firejail = {
      enable = true;
      wrappedBinaries = {
        zoom = {
          executable = "${lib.getBin pkgs.zoom-us}/bin/zoom-us";
          profile = "${pkgs.firejail}/etc/firejail/zoom.profile";
        };
      };
    };

    modules.bindings.items = [
      {
        description = "zoom";
        command = "/run/current-system/sw/bin/zoom";
      }
    ];
  };
}

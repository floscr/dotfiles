{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.zoom;
in
{
  options.modules.desktop.apps.zoom = {
    enable = mkBoolOpt false;
  };

  config = {
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

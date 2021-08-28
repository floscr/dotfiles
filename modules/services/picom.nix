# To mount android devices
{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.picom;
in
{
  options.modules.services.picom = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;


      backend = "glx";
      vSync = true;
      experimentalBackends = true;

      shadowExclude = [
        "name = 'bounding_shaped'"
        "name = 'cpt_frame_window'"
        "name = 'as_toolbar'"
        "name = 'zoom_linux_float_video_window'"
        "name = 'AnnoInputLinux'"
      ];

      # fade = true;
      # fadeDelta = 1;
      # fadeSteps = [ 1.0e-2 1.2e-2 ];

      shadow = true;
      shadowOffsets = [ (-10) (-10) ];
      shadowOpacity = 0.22;

      settings = {
        shadow-radius = 12;
        blur-background-exclude = [
          "name *= 'slop'"
          "name = 'cpt_frame_window'"
          "name = 'as_toolbar'"
          "name = 'zoom_linux_float_video_window'"
          "name = 'AnnoInputLinux'"
        ];
        blur-kern = "7x7box";
        blur-strength = 320;
      };
    };
  };
}

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
        "class_g = 'Dunst'"
        "!(_XMONAD_TAGS:s *= 'floating')"
      ];

      fade = true;
      fadeDelta = 1;
      fadeSteps = [ 1.0e-2 1.2e-2 ];
      fadeExclude = [
        "class_g != 'Dunst'"
      ];

      shadow = true;
      shadowOffsets = [ (-8) (-8) ];
      shadowOpacity = 0.15;

      settings = {
        shadow-radius = 20;
        shadow-exclude = [ "!(class_g = 'xmobar' || _XMONAD_TAGS:s *= 'floating')" ];

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

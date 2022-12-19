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

      shadowExclude = [
        "name = 'bounding_shaped'"
        "name = 'cpt_frame_window'"
        "name = 'as_toolbar'"
        "name = 'zoom_linux_float_video_window'"
        "name = 'AnnoInputLinux'"
        "class_g = 'Dunst'"
        "!(_XMONAD_TAGS:s *= 'floating')"
      ];

      # opacityRules = [
      #   "90:class_g = 'Alacritty' && !_NET_WM_STATE@:32a && _XMONAD_TAGS:s *= 'floating'"
      #   "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      # ];

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

        # xinerama-shadow-crop = true;
        # frame-opacity = 1;
        # inactive-opacity-override = false;
        # detect-client-opacity = true;
        # blur-backgroupnd = true;
        # blur-background-frame = true;
        # blur-background-fixed = false;
        # blur-background-exclude = [
        #   "window_type = 'dock'"
        #   "window_type = 'desktop'"
        # ];
        # mark-wmwin-focused = true;
        # mark-ovredir-focused = true;
        # use-ewmh-active-win = true;
        # detect-rounded-corners = true;
        # dbe = false;
        # unredir-if-possible = false;
        # detect-transient = true;
        # detect-client-leader = true;
        # xrender-sync-fence = true;
        # blur = {
        #   method = "kawase";
        #   strength = 1;
        #   background = true;
        #   background-frame = true;
        #   background-fixed = true;
        #   kern = "3x3box";
        # };

      };
    };
  };
}

{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.screen-capture;
in
{
  options.modules-new.scripts.screen-capture = with my; {
    enable = mkBoolOpt false;
    plugins = {
      single = {
        enable = mkBoolOpt true;
        dir = mkStrOpt "~/Media/Screenrecording";
      };
      record = {
        enable = mkBoolOpt true;
        dir = mkStrOpt "~/Media/Screenrecording";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.plugins.single.enable
      (
        let
          script-name = "screen-capture";
          package = (writeBabashkaScriptBin script-name ./src/screen_capture.clj);
          bin = "${package}/bin/${script-name}";
          dir = cfg.plugins.record.dir;
        in
        {
          user.packages = with pkgs; [
            package
            xclip
            dunst
            maim
            ffmpeg
            slop
            screenkey
          ];
          modules.bindings.items = [
            {
              xmonadBinding = "M-S-s";
              command = "${bin} static";
              description = "Screenshot";
            }
            {
              description = "Screen Capture: Record MP4";
              xmonadBinding = "M-C-s";
              command = "${bin} mp4";
            }
            {
              description = "Screen Capture: Record MP4 + Screenkey";
              xmonadBinding = "M-C-M1-s";
              command = "${bin} mp4 --screenkey";
            }
          ];
        }
      )
    )
  ]);
}

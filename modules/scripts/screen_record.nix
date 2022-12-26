{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.scripts.screen_record;
  script-name = "screen-record";
in
{
  options.modules.scripts.screen_record = {
    enable = my.mkBoolOpt false;
    capture-dir = mkStrOpt "~/Media/Screenrecording";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ffmpeg
      slop
      screenkey
      (pkgs.writeScriptBin script-name (builtins.readFile ./screen-record.zsh))
    ];

    modules.bindings.items = [
      {
        description = "Record Screen (MP4)";
        xmonadBinding = "M-C-s";
        command = "${script-name} -s ${cfg.capture-dir}/$(date +%F-%T).mp4";
      }
      {
        description = "Record Screen (MP4) + Screenkey";
        xmonadBinding = "M-C-M1-s";
        command = "${script-name} -s -w ${cfg.capture-dir}/$(date +%F-%T).mp4";
      }
      {
        description = "Record Screen (GIF)";
        xmonadBinding = "M-M1-s";
        command = "${script-name} -s ${cfg.capture-dir}/$(date +%F-%T).gif";
      }
    ];
  };
}

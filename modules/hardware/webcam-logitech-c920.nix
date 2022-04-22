{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio = {
    enable = mkBoolOpt false;
    device = mkStrOpt "/dev/video4";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      v4l-utils # Webcam utils
    ];

    systemd.user.services.set-webcam = {
      Unit = {
        Description = "Set the correct settings for the webcam using V4L.";
        Documentation = [ "man:v4l2-ctl(1)" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = ''
          ${pkgs.v4l-utils}/bin/v4l2-ctl \
            --device=${device} \
            --set-ctrl=focus_auto=0 \
            --set-ctrl=focus_absolute=0 \
            --set-ctrl=white_balance_temperature_auto=0 \
            --set-ctrl=contrast=85 \
            --set-ctrl=saturation=140 \
            --set-ctrl=white_balance_temperature=2850 \
            --set-ctrl=sharpness=100
        '';
      };
    };
  };
}

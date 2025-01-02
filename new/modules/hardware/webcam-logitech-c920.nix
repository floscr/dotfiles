{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.webcam.logitech-c920;
in
{
  options.modules.hardware.webcam.logitech-c920 = {
    enable = mkBoolOpt false;
    device = mkStrOpt "/dev/video4";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      v4l-utils # Webcam utils
    ];

    systemd.user.services.setup-webcam = {
      description = "Set the correct settings for the webcam using V4L.";
      documentation = [ "man:v4l2-ctl(1)" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = ''
          ${pkgs.v4l-utils}/bin/v4l2-ctl \
            --device=${cfg.device} \
            --set-ctrl=focus_auto=0 \
            --set-ctrl=focus_absolute=0 \
            --set-ctrl=contrast=85 \
            --set-ctrl=saturation=140 \
            --set-ctrl=white_balance_temperature=2850 \
            --set-ctrl=sharpness=200
        '';
      };
    };
  };
}

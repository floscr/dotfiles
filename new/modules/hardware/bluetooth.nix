{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  hwCfg = config.modules.hardware;
  cfg = hwCfg.bluetooth;
in
{
  options.modules.hardware.bluetooth = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hardware.bluetooth = {
        enable = true;
        package = pkgs.bluez5;
        powerOnBoot = true;
        settings = {
          General = {
            Enable = "Source,Sink,Media,Socket";
            ControllerMode = "bredr";
            # Disable headset profiles (HFP/HSP) to force A2DP for better audio quality
            Disable = "Headset";
            # Automatically enable and connect to trusted devices
            AutoEnable = "true";
            AutoConnect = "true";
            MultiProfile = "multiple";
          };
        };
      };

      user.packages = with pkgs; [
        bluez-tools
      ];
    }

    (mkIf cfg.audio.enable {
      services.pulseaudio = {
        # NixOS allows either a lightweight build (default) or full build of
        # PulseAudio to be installed.  Only the full build has Bluetooth
        # support, so it must be selected here.
        package = pkgs.pulseaudioFull;
        # Enable additional codecs and force A2DP
        extraConfig = ''
          # Disable HFP/HSP profiles to force A2DP for better audio quality
          load-module module-bluetooth-discover headset=auto
          .ifexists module-bluetooth-policy.so
          load-module module-bluetooth-policy auto_switch=2
          .endif
        '';
      };
    })
  ]);
}

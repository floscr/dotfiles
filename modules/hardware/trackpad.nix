{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.trackpad;
in {
  options.modules.hardware.trackpad = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver.synaptics = {
      enable = true;
      additionalOptions = ''
      Option "VertScrollDelta" "100"
      Option "HorizScrollDelta" "100"
      Option "ClickTime" "25"
      Option "Sensitivity" "1.4"
      Option "FingerHigh" "12"
      Option "FingerLow" "1"
      Option "BottomEdge" "30"
      Option "PalmSize" "45"
      Option "IgnoreThumb" "true"
    '';
      palmDetect = true;
      minSpeed = ".9";
      maxSpeed = "1.4";
      accelFactor = "0.005";
      buttonsMap = [ 1 2 3 ];
      fingersMap = [ 1 3 2 ];
      twoFingerScroll = true;
      vertEdgeScroll = false;
    };
  };
}

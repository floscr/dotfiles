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
    services.xserver.inputClassSections = [''
      Identifier "Apple Magic Trackpad"
      Driver "synaptics"
      # Match only the Apple Magic Trackpad
      MatchUSBID "05ac:030e"
      MatchIsTouchpad "on"
      # Set resolution tweaks for better response
      Option "VertResolution" "75"
      Option "HorizResolution" "75"
      # Set a timeout for multi finger click so accidental double-clicks don't
      # happen when right clicking and other gestures
      Option "EmulateMidButtonTime" "100"
      # Increase sensitivity
      Option "MinSpeed" "3.00"
      Option "MaxSpeed" "10.00"
      Option "AccelFactor" "0.2"
      # Scrolling
      Option "VertScrollDelta" "-100"
      Option "HorizScrollDelta" "-100"
    Option          "Pressure Calibration Offset" "30"
    Option          "MaxTapTime" "300"
    Option          "Palm Pressure" "250.0"
    Option          "Palm Width" "20.0"
    Option          "Multiple Palm Width" "20.0"

    # Enable Stationary Wiggle Filter
    Option          "Stationary Wiggle Filter Enabled" "1"
    Option          "Finger Moving Energy" "0.0008"
    Option          "Finger Moving Hysteresis" "0.0004"

    # Avoid accidental scroll/move on finger lift
    Option          "Max Stationary Move Speed" "47"
    Option          "Max Stationary Move Speed Hysteresis" "1"
    Option          "Max Stationary Move Suppress Distance" "0.2"
    ''];
  };
}

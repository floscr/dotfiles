# To mount android devices
{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.android;
in
{
  options.modules.services.android = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
    user.extraGroups = [ "adbusers" ];

    user.packages = with pkgs; [
      jmtpfs
    ];

    env = {
      ANDROID_USER_HOME = "$XDG_DATA_HOME/android";
      ANDROID_HOME = "$XDG_DATA_HOME/android/sdk";
      ANDROID_AVD_HOME = "$XDG_DATA_HOME/android/avd";
    };
  };
}

# To mount android devices
{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.android;
in {
  options.modules.services.android = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Dev tools
    programs.adb.enable = true;
    user.extraGroups = ["adbusers"];

    user.packages = with pkgs; [
      jmtpfs
    ];
  };
}

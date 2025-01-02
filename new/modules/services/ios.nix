{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.services.ios;
in
{
  options.modules.services.ios = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {

    services.usbmuxd = {
      enable = true;
      package = pkgs.unstable.usbmuxd2;
      user = "floscr";
    };

    environment.systemPackages = with pkgs; [
      libimobiledevice
      ifuse # optional, to mount using 'ifuse'
    ];
  };
}

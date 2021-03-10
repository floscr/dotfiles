{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shared.usbNetworkDetect;
in {
  options.modules.shared.usbNetworkDetect = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services = {
      udev.extraRules = ''
        SUBSYSTEM=="net", ENV{ID_BUS}=="usb", ENV{ID_USB_DRIVER}=="rndis_host", ACTION=="add", RUN{program}+="/bin/sh -c 'echo $env{INTERFACE} > /var/tmp/myfile'"
      '';
    };
  };
}

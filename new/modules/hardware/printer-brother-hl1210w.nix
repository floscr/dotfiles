{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.hardware.printer.brother.hl1210w;
in
{
  options.modules.hardware.printer.brother.hl1210w = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.printing = {
      enable = true;
      browsing = true;
      defaultShared = true;
      listenAddresses = [ "*:631" ];
      drivers = [ pkgs.cups-brother-hl1210w pkgs.brlaser ];
      allowFrom = [ "all" ];
      extraConf = ''
        DefaultPaperSize A4
      '';
    };

    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };

  };
}

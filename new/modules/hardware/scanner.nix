{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.scanner;
in
{
  options.modules.hardware.scanner = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      scannerPkgs.scantailor
    ];

    hardware.sane.enable = true;
    hardware.sane.extraBackends = with pkgs; [
      scannerPkgs.epkowa
      scannerPkgs.utsushi
    ];
    services.udev.packages = [ pkgs.scannerPkgs.utsushi ];
  };
}

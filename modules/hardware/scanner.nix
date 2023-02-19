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
      scantailor
    ];

    hardware.sane.enable = true;
    hardware.sane.extraBackends = with pkgs; [
      epkowa
      pkgs.utsuhiPkgs.utsushi
    ];
    services.udev.packages = [ pkgs.utsuhiPkgs.utsushi ];
  };
}

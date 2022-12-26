{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.gaming.emulators;
in
{
  options.modules.desktop.gaming.emulators = {
    n64.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.n64.enable {
      user.packages = with pkgs; [
        mupen64plus
      ];

      # Switch controller support
      boot.extraModulePackages = with config.boot.kernelPackages; [
        hid-nintendo
      ];
    })
  ];
}

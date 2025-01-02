{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.fs;
in
{
  options.modules.hardware.fs = {
    enable = mkBoolOpt false;
    ssd.enable = mkBoolOpt false;
    autoMount.enable = mkBoolOpt false;
    gui.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Support for more filesystems, mostly to support external drives
      environment.systemPackages = with pkgs; [
        sshfs
        exfat
        ntfs3g
        hfsprogs
      ];

      # Mounting daemon
      programs.udevil.enable = true;

      # SSD
      services.fstrim.enable = true;
    }

    (mkIf (cfg.gui.enable) {
      user.packages = with pkgs; [
        gparted
      ];
    })

    (mkIf (cfg.autoMount.enable) {
      user.packages = [ pkgs.unstable.udiskie ];
      services.udisks2.enable = true;

      home-manager.users.${config.user.name}.services.udiskie = {
        enable = true;
        automount = true;
        tray = "never";
      };
    })
  ]);
}

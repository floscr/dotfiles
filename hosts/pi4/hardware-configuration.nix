{ config, lib, pkgs, modulesPath, inputs, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi.4
    ];

    fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

    # Enable GPU acceleration
    boot.loader.raspberryPi.firmwareConfig = ''
    gpu_mem=320
  '';
    hardware.raspberry-pi."4".fkms-3d = {
    enable = true;
    cma = 512;
  };

    }

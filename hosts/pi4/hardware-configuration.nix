{ config, lib, pkgs, modulesPath, inputs, ... }:

{
  imports = [
    "${fetchTarball "https://github.com/NixOS/nixos-hardware/archive/936e4649098d6a5e0762058cb7687be1b2d90550.tar.gz"}/raspberry-pi/4"
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

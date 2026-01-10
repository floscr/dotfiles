{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Latest kernel for best AMD support
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  # Bootloader (UEFI)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c16ca1e5-c859-4010-923d-c36d0937550f";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/4E2E-9B9D";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [ ];

  # CPU - AMD Ryzen 7 5800X
  nix.settings.max-jobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # GPU - AMD Radeon RX 6800 (Navi 21 / RDNA2)
  services.xserver.videoDrivers = [ "amdgpu" ];
  hardware.amdgpu.initrd.enable = true;

  hardware.graphics = {
    enable = true;
    enable32Bit = true; # For Steam/gaming
    extraPackages = with pkgs; [
      vulkan-loader
      libva
    ];
  };

  # Firmware
  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    fwupd
  ];
  services.fwupd.enable = true; # BIOS/firmware updates
}

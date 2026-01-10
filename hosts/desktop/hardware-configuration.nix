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

  # LUKS encryption
  boot.initrd.luks.devices = {
    cryptroot = {
      device = "/dev/disk/by-uuid/1a04b632-8291-45e7-a2c0-54799e73ffa4";
      preLVM = true;
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3b3183bb-8bf0-47f3-bfc0-427a3dcca901";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/F77D-675B";
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

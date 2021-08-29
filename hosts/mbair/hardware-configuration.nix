{ config, lib, inputs, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.extraModulePackages = with config.boot.kernelPackages; [ broadcom_sta mba6x_bl ];
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "nvme"
    "usb_storage"
    "usbhid"
    "sd_mod"
    "mba6x_bl"
  ];
  # boot.initrd.kernelModules = [ "fbcon" ];
  boot.kernelModules = [ "kvm-intel" "wl" "mba6x_bl" ];
  # Divides power consumption by two.
  boot.kernelParams = [ "acpi_osi=" ];

  services.xserver.deviceSection = lib.mkDefault ''
    Option "Backlight" "mba6x_backlight"
    Option "TearFree" "true"
  '';

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/b88d7012-d417-458a-8524-96388bc685df";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/4BE8-B673";
      fsType = "vfat";
    };

  fileSystems."/home" =
    {
      device = "/dev/disk/by-uuid/239c54b1-7cbc-418a-a097-691d5df07aef";
      fsType = "ext4";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/7178846e-6580-47e8-8a1c-52398464c0e9"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # hardware.opengl = {
  #   enable = true;
  #   extraPackages = with pkgs; [
  #     vaapiIntel
  #     vaapiVdpau
  #     libvdpau-va-gl
  #   ];
  #   driSupport32Bit = true;
  # };

}

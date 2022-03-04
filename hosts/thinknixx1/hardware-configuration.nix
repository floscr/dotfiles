{ config, lib, pkgs, modulesPath, inputs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    # inputs.nixos-hardware.nixosModules.common-cpu-intel
    # inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];

  # Remove screen tearing
  environment.etc."X11/xorg.conf.d/20-intel.conf" = {
    text = ''
      Section "Device"
        Identifier "Intel Graphics"
        Driver "intel"
        Option "TearFree" "true"
        Option "AccelMethod" "sna"
        Option "SwapbuffersWait" "true"
        Option "TripleBuffer" "true"
        Option "VariableRefresh" "true"
        Option "DRI" "2"
      EndSection
    '';
  };

  environment.systemPackages = with pkgs; [
    fwupd
    undervolt
  ];
  services.fwupd.enable = true; # Bios updates

  # CPU
  nix.settings.max-jobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Harddrives
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    setLdLibraryPath = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl

      # Vulkan
      amdvlk
      vulkan-loader
    ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/55904fe9-fcc7-49e8-8839-8505347d9084";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/FF89-C091";
    fsType = "vfat";
  };

  swapDevices = [{
    device = "/dev/disk/by-uuid/472dca10-b608-4cbc-83f1-2f8980b8a620";
  }];
}

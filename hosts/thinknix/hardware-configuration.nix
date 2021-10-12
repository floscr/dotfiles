{ config, lib, pkgs, modulesPath, inputs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelModules = [ "kvm-intel" "acpi_call" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  boot.kernelParams = [
    "i915.enable_psr=1"
    "i915.enable_guc=2"
    "i915.nuclear_pageflip=Y"
    "i915.enable_dc=2"
  ];

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
  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  hardware.cpu.intel.updateMicrocode = true;

  # Graphics
  services.xserver.videoDrivers = [ "intel" ];
  # services.xserver.videoDrivers = [ "modesetting" ];
  services.xserver.useGlamor = true;
  services.xserver = {
    deviceSection = ''
      Option "VariableRefresh" "true"
      Option "DRI" "3"
      Option "TearFree" "True"
    '';
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

  # Battery
  services.upower.enable = true;
  services.tlp = {
    enable = true;
    extraConfig = ''
      START_CHARGE_THRESH_BAT0=75
      STOP_CHARGE_THRESH_BAT0=92
      CPU_SCALING_GOVERNOR_ON_AC=performance
      CPU_BOOST_ON_AC=1
      CPU_SCALING_GOVERNOR_ON_BAT=powersave
      ENERGY_PERF_POLICY_ON_BAT=powersave
    '';
  };

  # Fan Control
  boot.extraModprobeConfig = "options thinkpad_acpi experimental=1 fan_control=1";
  ## Fanspeed
  # https://gist.github.com/Yatoom/1c80b8afe7fa47a938d3b667ce234559
  services.thinkfan.enable = true;
  services.thinkfan.smartSupport = true;
  services.thinkfan.levels = [
    [ 0 0 68 ]
    [ 1 60 65 ]
    [ 2 65 70 ]
    [ 3 70 75 ]
    [ 4 75 80 ]
    [ 5 80 85 ]
    [ 6 85 90 ]
    [ 7 90 95 ]
    [ 127 95 32767 ]
  ];

  systemd.services."thinkfan-restart-resume" = {
    description = "Restart thinkfan after suspend.";
    after = [ "suspend.target" ];
    wantedBy = [ "suspend.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.sudo}/bin/sudo ${pkgs.systemd}/bin/systemctl restart thinkfan.service";
    };
  };

  # Undervolting & Throttling
  services.throttled = {
    enable = true;
    extraConfig = ''
      [GENERAL]
      # Enable or disable the script execution
      Enabled: True
      # SYSFS path for checking if the system is running on AC power
      Sysfs_Power_Path: /sys/class/power_supply/AC*/online

      ## Settings to apply while connected to AC power
      [AC]
      # Update the registers every this many seconds
      Update_Rate_s: 10
      # Max package power for time window #1
      PL1_Tdp_W: 42
      # Time window #1 duration
      PL1_Duration_s: 28
      # Max package power for time window #2
      PL2_Tdp_W: 44
      # Time window #2 duration
      PL2_Duration_S: 0.002
      # Max allowed temperature before throttling
      Trip_Temp_C: 66
      # Set HWP energy performance hints to 'performance' on high load (EXPERIMENTAL)
      HWP_Mode: True
      # Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)
      cTDP: 2

      [UNDERVOLT.AC]
      # CPU core voltage offset (mV)
      CORE: -95
      # Integrated GPU voltage offset (mV)
      GPU: -85
      # CPU cache voltage offset (mV)
      CACHE: -105
      # System Agent voltage offset (mV)
      UNCORE: -85
      # Analog I/O voltage offset (mV)
      ANALOGIO: 0

      ## Settings to apply while connected to Battery power
      [BATTERY]
      # Update the registers every this many seconds
      Update_Rate_s: 10
      # Max package power for time window #1
      PL1_Tdp_W: 42
      # Time window #1 duration
      PL1_Duration_s: 28
      # Max package power for time window #2
      PL2_Tdp_W: 44
      # Time window #2 duration
      PL2_Duration_S: 0.002
      # Set HWP energy performance hints to 'performance' on high load (EXPERIMENTAL)
      HWP_Mode: True
      # Max allowed temperature before throttling
      Trip_Temp_C: 66
      # Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)
      cTDP: 2

      [UNDERVOLT.BATTERY]
      # CPU core voltage offset (mV)
      CORE: -95
      # Integrated GPU voltage offset (mV)
      GPU: -85
      # CPU cache voltage offset (mV)
      CACHE: -105
      # System Agent voltage offset (mV)
      UNCORE: -85
      # Analog I/O voltage offset (mV)
      ANALOGIO: 0

      # [ICCMAX.AC]
      # # CPU core max current (A)
      # CORE:
      # # Integrated GPU max current (A)
      # GPU:
      # # CPU cache max current (A)
      # CACHE:

      # [ICCMAX.BATTERY]
      # CPU core max current (A)
      # CORE:
      # # Integrated GPU max current (A)
      # GPU:
      # # CPU cache max current (A)
      # CACHE:
    '';
  };

  # Harddrives
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/1256e32a-46cd-443e-9bb7-8fd910cc5b32";
      fsType = "ext4";
    };
  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/18B0-385A";
      fsType = "vfat";
    };
  swapDevices =
    [{ device = "/dev/disk/by-uuid/03ad0fe9-be47-438f-a5c0-88eca0287fde"; }];
}

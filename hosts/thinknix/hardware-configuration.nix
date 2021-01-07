{ config, lib, pkgs, modulesPath, inputs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    # inputs.nixos-hardware.common.cpu.intel
    # inputs.nixos-hardware.common.pc.acpi_call
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" "acpi_call" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

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
  services.xserver.videoDrivers = ["intel"];

  ## Integrated graphics driver
  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };
  hardware.opengl.package = (pkgs.mesa.override {
    galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
  }).drivers;
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    driSupport = true;
    extraPackages32 = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
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
  services.thinkfan.levels = ''
        { "level 0"
            (0 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (68 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 1"
            (60 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (65 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 2"
            (65 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (70 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 3"
            (70 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (75 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 4"
            (75 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (80 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 5"
            (80 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (85 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 6"
            (85 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (90 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level 7"
            (90 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (95 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

        { "level disengaged" # nice idea: "level auto" can also be used.
                             # but again: only numbers for sysfs.
            (95 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
            (32767 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .)
        }

  '';

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
    { device = "/dev/disk/by-uuid/1256e32a-46cd-443e-9bb7-8fd910cc5b32";
      fsType = "ext4";
    };
  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/18B0-385A";
      fsType = "vfat";
    };
  swapDevices =
    [ { device = "/dev/disk/by-uuid/03ad0fe9-be47-438f-a5c0-88eca0287fde"; }
    ];
}

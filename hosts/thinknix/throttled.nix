# Sometimes I want max performance and don't care about the fan going at full blast
{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
{
  systemd.services."lenovo_fix_performance" = {
    description = "Intel Throttling Performance Mode";
    environment = {
      PYTHONBUFFERED = "1";
    };
    conflicts = [ "lenovo_fix.service" "thinkfan.service" ];
    serviceConfig = {
      ExecStart = "${pkgs.throttled}/bin/lenovo_fix.py --config ${config.environment.etc."lenovo_fix_performance.conf".source.outPath}";
      ExecStop = [
        "${pkgs.sudo}/bin/sudo ${pkgs.systemd}/bin/systemctl start lenovo_fix.service"
        "${pkgs.sudo}/bin/sudo ${pkgs.systemd}/bin/systemctl start thinkfan.service"
      ];
    };
  };

  modules.bindings.items = [
      {
          description = "Performance Mode On";
          categories = "Script";
          command = "sudo ${pkgs.systemd}/bin/systemctl start lenovo_fix_performance.service";
      }
      {
          description = "Performance Mode Off";
          categories = "Script";
          command = "sudo ${pkgs.systemd}/bin/systemctl stop lenovo_fix_performance.service";
      }
  ];

  security.sudo.enable = true;
  security.sudo.extraConfig = ''
    %wheel      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl start lenovo_fix_performance.service
    %wheel      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl stop lenovo_fix_performance.service
  '';

  environment.etc."lenovo_fix_performance.conf".text = ''
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
}

{ config, lib, ... }:

with lib;
{
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false
  # here. Per-interface useDHCP will be mandatory in the future, so this
  # generated config replicates the default behaviour.
  networking.useDHCP = false;

  location = {
    latitude = 12.5;
    longitude = 55.88;
  };

  time.timeZone = mkDefault "Europe/Vienna";
}

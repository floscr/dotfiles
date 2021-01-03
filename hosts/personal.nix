{ config, lib, ... }:

with lib;
{
  networking.networkmanager.enable = true;

  time.timeZone = mkDefault "Europe/Vienna";
}

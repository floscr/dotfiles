{ config, lib, ... }:

with lib;
{
  networking.networkmanager.enable = true;
}

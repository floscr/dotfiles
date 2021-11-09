{ config, lib, pkgs, ... }:

{
  modules.bindings.items = [
    {
      description = "Nautilus";
      command = "nautilus";
    }
    {
      description = "Boomer";
      command = "${pkgs.user.boomer}/bin/boomer";
    }
  ];
}

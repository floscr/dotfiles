{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    services.xserver.displayManager.sessionCommands = ''
      # Fix default cursor
      xsetroot -cursor_name left_ptr
    '';
  };
}

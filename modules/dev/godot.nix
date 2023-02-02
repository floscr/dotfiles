{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.godot;
in
{
  options.modules.dev.godot = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      godot
    ];
  };
}

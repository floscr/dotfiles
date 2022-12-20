{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.graphics.inkscape;
in
{
  options.modules.desktop.graphics.inkscape = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      inkscape
    ];
  };
}

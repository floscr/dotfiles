{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.color-optimization;
in
{
  options.modules.shell.color-optimization = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      libjpeg
      optipng
      gifsicle
    ];
  };
}

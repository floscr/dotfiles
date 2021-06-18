# A system monitoring tool for X11
# https://github.com/aristocratos/bpytop
{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.bpytop;
in {
  options.modules.shell.bpytop = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.bpytop ];

    home.configFile = {
      "bpytop" = {
        source = "${configDir}/bpytop";
        recursive = true;
      };
    };
  };
}

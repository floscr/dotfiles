{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.sysz;
in
{
  options.modules.shell.sysz = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      sysz
    ];

    modules.shell.zsh = {
      aliases = {
        sz = "sysz";
      };
    };
  };
}

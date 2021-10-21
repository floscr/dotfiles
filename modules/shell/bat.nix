{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.bat;
in
{
  options.modules.shell.bat = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      bat
    ];

    modules.shell.zsh =
      {
        aliases = {
          b = "bat";
        };
      };
  };
}

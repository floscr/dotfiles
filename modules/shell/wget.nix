{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.wget;
in
{
  options.modules.shell.wget = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.wget ];

    environment.variables = {
      WGETRC = "$XDG_CONFIG_HOME/wget/wgetrc";
    };

    home.configFile = {
      "wget" = {
        source = "${configDir}/wget";
        recursive = true;
      };
    };
  };
}

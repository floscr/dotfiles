{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.mysql;
in
{
  options.modules.dev.mysql = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.mysql = {
      enable = true;
      package = pkgs.mariadb;
    };

    environment.sessionVariables = {
      MYSQL_HISTFILE = "$XDG_CACHE_HOME/mysql_history";
    };
  };
}

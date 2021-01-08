{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.graphics.gimp;
in {
  options.modules.desktop.graphics.gimp = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      gimp
      gimpPlugins.resynthesizer  # content-aware scaling in gimp
    ];
    home.configFile = mkIf cfg.enable {
      "GIMP/2.10" = { source = "${configDir}/gimp"; recursive = true; };
    };
  };
}

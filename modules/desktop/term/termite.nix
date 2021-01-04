{ config, options, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.termite;
    inherit (config.modules.theme) fonts;
in {
  options.modules.desktop.term.termite = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      termite
      (makeDesktopItem {
        name = "termite";
        desktopName = "Termite";
        genericName = "Default terminal";
        icon = "utilities-terminal";
        exec = "${termite}/bin/termite";
        categories = "Development;System;Utility";
      })
    ];

    home-manager.users.${config.user.name}.programs.termite = {
        enable = true;
        font = "${fonts.mono.family} 12";
        scrollbackLines = -1;
        allowBold = true;
        clickableUrl = true;
        cursorBlink = "off";
        dynamicTitle = true;
        geometry = "81x20";
        mouseAutohide = true;
      };
      # (mkIf config.modules.theme.colors {})
  };
}

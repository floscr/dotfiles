{ config, options, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.termite;
  inherit (config.modules.theme) fonts;
in
{
  options.modules.desktop.term.termite = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.theme.onReload.termite = ''
      pkill -USR1 -x termite
    '';

    user.packages = with pkgs; [
      termite
      # (makeDesktopItem {
      #   name = "termite";
      #   desktopName = "Termite";
      #   genericName = "Default terminal";
      #   icon = "utilities-terminal";
      #   exec = "${termite}/bin/termite";
      #   categories =[ "Development;System;Utility"];
      # })
    ];

    home-manager.users.${config.user.name}.programs.termite = mkMerge [
      {
        enable = true;
        font = "${fonts.terminal.family} ${toString fonts.terminal.size}";
        scrollbackLines = -1;
        allowBold = true;
        clickableUrl = true;
        cursorBlink = "off";
        dynamicTitle = true;
        geometry = "81x20";
        mouseAutohide = true;
      }
      (mkIf (config.modules.theme.colorscheme != null) (with config.modules.theme.colors; {
        backgroundColor = bg0;
        foregroundColor = fg0;
        colorsExtra = ''
          color0  = ${bg0}
          color7  = ${fg0}
          color8  = ${fg1}
          color1  = ${alert}
          color9  = ${quaternary}
          color2  = ${secondary}
          color10 = ${fg2}
          color3  = ${quaternary}
          color11 = ${quinary}
          color4  = ${primary}
          color12 = ${septary}
          color5  = ${senary}
          color13 = ${tertiary}
          color6  = ${primary}
          color14 = ${septary}
          color15 = ${fg0}
        '';
      }))
    ];
  };
}

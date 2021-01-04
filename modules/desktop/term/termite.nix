{ config, options, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.termite;
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
        font = "${config.modules.theme.fonts.monoSpacePrimary} 8";
        scrollbackLines = -1;
        allowBold = true;
        clickableUrl = true;
        cursorBlink = "off";
        dynamicTitle = true;
        geometry = "81x20";
        mouseAutohide = true;
      }
      (mkIf config.modules.theme.colors != null (with config.modules.theme.colors; {
        backgroundColor = terminalBackground;
        foregroundColor = text;
        colorsExtra = ''
          color0  = ${terminalBackground}
          color7  = ${text}
          color8  = ${grey2}
          color1  = ${red}
          color9  = ${bred}
          color2  = ${grn}
          color10 = ${bgrn}
          color3  = ${yellow}
          color11 = ${byellow}
          color4  = ${blue}
          color12 = ${bblue}
          color5  = ${mag}
          color13 = ${bmag}
          color6  = ${cyn}
          color14 = ${bcyn}
          color15 = ${white}
        '';
      }))
    ]);
  };
}

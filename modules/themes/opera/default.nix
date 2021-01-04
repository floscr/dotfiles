{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  config = mkIf (cfg.active == "opera") (mkMerge [
    {
      modules = {
        theme = {
          colors = {
            black1 = "#141517";
            black2 = "#191c21";
            white = "#ffffff";
            grey1 = "#c5c8c6";
            grey1blue = "#d8dee9";
            grey2 = "#969896";
            red = "#cc6666";
            bred = "#de935f";
            grn = "#b5bd68";
            bgrn = "#757d28";
            yellow = "#f0c674";
            byellow = "#f9a03f";
            blue = "#81a2be";
            bblue = "#2a8fed";
            mag = "#b294bb";
            bmag = "#bc77a8";
            cyn = "#8abeb7";
            bcyn = "#a3685a";
            # Aliases
            terminalBackground = cfg.colors.black1;
            background = cfg.colors.black2;
            success = cfg.colors.grn;
            fail = cfg.colors.red;
            text = cfg.colors.grey1;
          };
        };
      };
    }
  ]);
}

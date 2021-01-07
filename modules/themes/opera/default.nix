{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  config = mkIf (cfg.active == "opera") (mkMerge [
    {
      modules = {
        theme = {
          colorscheme = "gotham";
          fonts = rec {
            sans = {
              family = "Source Sans Pro";
              size = 8;
              pkg = pkgs.source-sans-pro;
            };
            serif = {
              family = "Source Serif Pro";
              size = 8;
              pkg = pkgs.source-serif-pro;
            };
            mono = {
              family = "Iosevka";
              size = 8;
              pkg = pkgs.iosevka;
            };
            ui = sans;
          };
        };
      };
    }
  ]);
}

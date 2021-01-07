{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  config = mkIf (cfg.active == "opera") (mkMerge [
    {
      modules = {
        theme = {
          colorscheme = "nord";

          gtk = {
            theme = "Arc-Dark";
            iconTheme = "Paper";
            cursorTheme = "McMojave";
          };

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

        shell.zsh.rcFiles  = [ ./config/zsh/prompt.zsh ];
      };
    }

    (mkIf config.services.xserver.enable {
      user.packages = with pkgs; [
        arc-theme
        paper-icon-theme # for rofi
      ];
    })

    {
      home.configFile = with config.modules; mkMerge [
        {
          # McMojave cursor theme
          "icons" = { source = ./icons/cursor; recursive = true; };
        }
      ];
    }
  ]);
}

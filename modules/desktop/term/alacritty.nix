{ config, options, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.term.alacritty;
  theme = config.modules.theme;
  inherit (config.modules.theme) fonts;
in
{
  options.modules.desktop.term.alacritty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      alacritty
    ];

    home-manager.users.${config.user.name}.programs.alacritty = {
      enable = true;
      settings = {
        env.TERM = "xterm-256color";
        colors = (with theme.colours; {
          primary = {
            background = bg;
            foreground = fg;
          };
          cursor = {
            text = fgalt;
            cursor = fg;
          };
          normal = {
            black = bg;
            red = red;
            green = green;
            yellow = yellow;
            blue = blue;
            magenta = magenta;
            cyan = cyan;
            white = fg;
          };
          bright = {
            black = bgalt;
            red = orange;
            green = teal;
            yellow = orange;
            blue = darkblue;
            magenta = violet;
            cyan = darkcyan;
            white = fgalt;
          };
        });
        font = {
          normal = {
            family = fonts.terminal.family;
            style = "Medium";
          };
          size = fonts.terminal.size;
        };
        keybindings = [
          { key = "Escape"; mode = "Vi"; action = "ClearSelection"; }
          { key = "y"; mode = "Vi"; action = "Copy"; }
          { key = "y"; mode = "Vi"; action = "ClearSelection"; }
          { key = "N"; mods = "Super"; action = ''SpawnNewInstance''; }
        ];
        window = {
          dimensions = {
            columns = 81;
            lines = 24;
          };
          padding = {
            x = 12;
            y = 12;
          };
          dynamic_title = true;
        };
        scrolling = {
          history = 99999;
          multiplier = 3;
        };
      };
    };
  };
}

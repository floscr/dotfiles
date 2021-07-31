{ config, options, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.alacritty;
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
        colors = (with config.modules.theme.colors; {
          primary = {
            background = bg0;
            foreground = fg0;
          };
          cursor = {
            text = fg0;
            cursor = fg0;
          };
          normal = {
            black = bg0;
            red = alert;
            green = secondary;
            yellow = quinary;
            blue = primary;
            magenta = tertiary;
            cyan = senary;
            white = fg1;
          };
          bright = {
            black = bg1;
            red = quaternary;
            green = fg2;
            yellow = quaternary;
            blue = primary;
            magenta = senary;
            cyan = senary;
            white = fg0;
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
        env.TERM = "xterm-256color";
        window = {
          dimensions = {
            columns = 81;
            lines = 24;
          };
          padding = {
            x = 12;
            y = 12;
          };
        };
        scrolling = {
          history = 10000;
          multiplier = 3;
        };
      };
    };
  };
}

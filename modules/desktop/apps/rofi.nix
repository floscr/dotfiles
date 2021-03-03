{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.rofi;
in {
  options.modules.desktop.apps.rofi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.configFile = {
      "rofi" = {
        source = "${configDir}/rofi";
        recursive = true;
      };
    };

    user.packages = with pkgs; [
      user.frece # Maintain a database sorted by frecency
      (writeScriptBin "rofi" ''
        #!${stdenv.shell}
        exec ${rofi}/bin/rofi -terminal xst -m -1 "$@"
      '')
      rofimoji
    ];
    modules.bindings.items = [
      {
        binding = "super + @space";
        command = "nimx cmder";
        description = "Rofi Main Menu";
      }
      {
        binding = "super + shift + Tab";
        command = "/etc/dotfiles/bin/rofi/app_switch";
        description = "Switch application";
      }
      {
        description = "drun";
        categories = "Rofi drun";
        command = "rofi -modi drun -show drun";
      }
      {
        description = "Rofi Emoji Picker";
        categories = "Rofi drun";
        command = "rofimoji";
      }
    ];
  };
}

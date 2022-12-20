{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.rofi;
in
{
  options.modules.desktop.apps.rofi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (mkMerge [
      {
        home.configFile = {
          "rofi" = {
            source = "${configDir}/rofi";
            recursive = true;
          };
        };

        user.packages = with pkgs; [
          (writeScriptBin "rofi" ''
            #!${stdenv.shell}
            exec ${rofi}/bin/rofi -terminal xst -m -1 "$@"
          '')
          rofimoji
          # user.rofi-translate
          rofi-systemd
        ];
        modules.bindings.items = [
          {
            binding = "super + @space";
            command = "rofi_cmder";
            description = "Rofi Main Menu";
          }
          {
            binding = "super + p";
            xmonadBinding = "M-p";
            command = "rofi_cmder";
            description = "Rofi Main Menu";
          }
          {
            binding = "super + shift + Tab";
            xmonadBinding = "M-S-<Tab>";
            command = "${config.dotfiles.binDir}/rofi/app_switch";
            description = "Switch application";
          }
          {
            xmonadBinding = "M1-<Tab>";
            command = "${config.dotfiles.binDir}/rofi/app_switch t";
            description = "Switch application (Current Desktop)";
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
          {
            description = "Rofi Translate";
            categories = "Rofi drun";
            command = "${pkgs.user.rofi-translate}/bin/rofi_trans";
          }
          {
            description = "Org Bookmarks";
            command = "rofi_org_bookmarks";
          }
          {
            description = "Systemd";
            command = "rofi-systemd";
          }
        ];
      }
      (mkIf (config.modules.shell.pass.enable == true) {
        user.packages = with pkgs; [
          rofi-pass
        ];

        home.configFile =
          {
            "rofi-pass/config".text = ''
              # Fix rofi error https://github.com/carnager/rofi-pass/issues/226
              help_color="#4872FF"
            '';
          };

        modules.bindings.items = [
          {
            binding = "super + apostrophe";
            command = "rofi-pass -dmenu -theme theme/passmenu.rasi";
            description = "Password Manager";
          }
        ];
      })
    ]);
}

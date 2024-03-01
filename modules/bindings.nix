{ config, options, lib, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.bindings;
  getOrDefault = str: otherwise: if str == "" then otherwise else str;
  generateJson = items: builtins.toJSON {
    value = "Commands";
    items = map
      (item: {
        value = getOrDefault item.description item.command;
        shell = item.command;
        action = getOrDefault item.action "exit";
      })
      items;
  };
in
{
  options.modules.bindings = with types; {
    enable = mkBoolOpt false;
    items = mkOption {
      type = with types; listOf (submodule ({ name, ... }: {
        options.binding = mkOption { type = nullOr str; default = null; };
        options.xmonadBinding = mkOption { type = nullOr str; default = null; };
        options.command = mkOption { type = str; default = ""; };
        options.description = mkOption { type = str; default = ""; };
        options.categories = mkOption { type = str; default = ""; };
        options.action = mkOption { type = str; default = ""; };
      }));
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    home.configFile."rofi_cmder/commands.json".text =
      builtins.toJSON (config.modules.bindings.items);
    home.configFile."iced_prompt/commands.json".text =
      generateJson (config.modules.bindings.items);
  };
}

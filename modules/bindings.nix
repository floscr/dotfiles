{ config, options, lib, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.bindings;
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
      }));
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    home.configFile."rofi_cmder/commands.json".text =
      builtins.toJSON (config.modules.bindings.items);
  };
}

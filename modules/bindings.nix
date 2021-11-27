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
    # Rofi CMDer
    home.configFile."rofi_cmder/commands.json".text =
      builtins.toJSON (config.modules.bindings.items);
    home.configFile."cmder/cmd.csv".text =
      fold
        (cur: acc:
          if isNull cur.description
          then acc
          else acc + "${cur.description},,,${cur.command},,,${if ! isNull(cur.binding) then cur.binding else ""}\n") ""
        config.modules.bindings.items;
    # home.configFile."sxhkd/sxhkdrc".text =
    #   fold
    #     (cur: acc: if isNull cur.binding then acc else ''
    #       ${acc}
    #       # ${cur.description}
    #       ${cur.binding}
    #           ${cur.command}
    #     '') ""
    #     config.modules.bindings.items;
  };
}

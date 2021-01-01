{ config, options, lib, home-manager, ... }:

with lib;
with lib.my;
{
  options = with types; {
    bindings = mkOption {
      type = with types; listOf (submodule({ name, ... }: {
        options.binding = mkOption { type = nullOr str; default = null; };
        options.command = mkOption { type = str; default = ""; };
        options.description = mkOption { type = str; default = ""; };
        options.categories = mkOption { type = str; default = ""; };
      }));
      default = [];
    };
  };

  config = {
#     # Rofi CMDer
#     home.configFile."cmder/cmd.csv"=
#       fold (cur: acc: acc + "${cur.description},,,${cur.command},,,${if ! isNull(cur.binding) then cur.binding else ""}\n") "" config.bindings;
#     # Sxhkd bindings
#     home.configFile."sxhkd/sxhkdrc" =
#       fold (cur: acc: if isNull cur.binding then acc else ''
# ${acc}
# # ${cur.description}
# ${cur.binding}
#     ${cur.command}
# '') "" config.bindings;
  };
}

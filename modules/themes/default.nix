{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  options.modules.theme = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v: let theme = builtins.getEnv "THEME"; in
                 if theme != "" then theme else v;
      description = ''
        Name of the theme to enable. Can be overridden by the THEME environment
        variable. Themes can also be hot-swapped with 'hey theme $THEME'.
      '';
    };

    colors = mkOption {
      type = with types; nullOr submodule({ name, ... }: {
        options.black1 = str;
        options.black2 = str;
        options.white = str;
        options.grey1 = str;
        options.grey1blue = str;
        options.grey2 = str;
        options.red = str;
        options.bred = str;
        options.grn = str;
        options.bgrn = str;
        options.yellow = str;
        options.byellow = str;
        options.blue = str;
        options.bblue = str;
        options.mag = str;
        options.bmag = str;
        options.cyn = str;
        options.bcyn = str;

        # Aliases
        options.terminalBackground = str;
        options.background = str;
        options.text = str;
        options.fail = str;
        options.success = str;
      });
      default = null;
    };

    onReload = mkOpt (attrsOf lines) {};
  };

  config = mkIf (cfg.active != null) (mkMerge [
  ]);
}

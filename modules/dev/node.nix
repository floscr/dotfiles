{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.node;
in {
  options.modules.dev.node = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nodejs-14_x
      yarn
    ];

    env.ADBLOCK               = "1"; # Stop node packages from showing ads in my darn CLI...
    env.NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
    env.NPM_CONFIG_CACHE      = "$XDG_CACHE_HOME/npm";
    env.NPM_CONFIG_TMP        = "$XDG_RUNTIME_DIR/npm";
    env.NPM_CONFIG_PREFIX     = "$XDG_CACHE_HOME/npm";
    env.NODE_REPL_HISTORY     = "$XDG_CACHE_HOME/node/repl_history";
    env.PATH = [ "$(yarn global bin)" ];

    # Run locally installed bin-script, e.g. n coffee file.coffee
    environment.shellAliases = {
      n  = "PATH=\"$(npm bin):$PATH\"";
      ya = "yarn";
    };

    home.configFile."npm/config".text = ''
      cache=$XDG_CACHE_HOME/npm
      prefix=$XDG_DATA_HOME/npm
      update-notifier=false # I'll update via nix, thank you.
    '';
  };
}

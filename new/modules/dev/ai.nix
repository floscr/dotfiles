{ options, config, pkgs, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.ai;
in
{
  options.modules.dev.ai = {
    enable = mkBoolOpt false;
    gemini.enable = mkBoolOpt false;
    anthropic.enable = mkBoolOpt false;
    codex.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.gemini.enable) { })
    (mkIf (cfg.anthropic.enable) { })
    (mkIf (cfg.codex.enable) {
      env.CODEX_HOME = "$XDG_CONFIG_HOME/codex";
    })
  ]);
}

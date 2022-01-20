{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.say;
in
{
  options.modules.shell.say = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      espeak
    ];

    modules.shell.zsh =
      {
        aliases = {
          say = "espeak";
        };
      };
  };
}

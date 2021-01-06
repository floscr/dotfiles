{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.weechat;
in {
  options.modules.shell.weechat = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      weechat
    ];
    env.WEECHAT_HOME = "$XDG_CONFIG_HOME/weechat";
  };
}

{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.elixir;
in
{
  options.modules.dev.elixir = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      elixir
      postgresql
      inotify-tools
    ];
  };
}

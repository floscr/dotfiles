{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.nim;
in
{
  options.modules.dev.nim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.variables = {
      NIM_UNWRAPPED_DIR = "${pkgs.nim-unwrapped}";
      NIMBLE_DIR = "~/.cache/.nimble";
    };
    user.packages = with pkgs; [
      nim
      nimlsp
    ];
  };
}

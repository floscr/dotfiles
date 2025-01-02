{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.dev.arduino;
in
{
  options.modules.dev.arduino = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {

    user.packages = with pkgs; [
      arduino
      arduino-cli
    ];
  };
}

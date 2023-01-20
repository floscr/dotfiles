{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.hey;
in
{
  options.modules-new.scripts.hey = with my; {
    enable = mkBoolOpt true;
  };

  config = {
    user.packages = with pkgs; [
      (writeBb "hey" {
        content = ./hey.clj;
        deps = [
          nvd
        ];
      })
    ];
  };
}

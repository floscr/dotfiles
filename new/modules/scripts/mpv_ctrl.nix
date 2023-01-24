{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.mpv_ctrl;
in
{
  options.modules-new.scripts.mpv_ctrl = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "mpv_ctrl" {
          content = ./src/mpv_ctrl.clj;
        });
      in
      {
        user.packages = with pkgs; [
          pkg
        ];
      }
    );
}

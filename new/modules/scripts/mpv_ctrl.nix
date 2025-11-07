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
          content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/mpv_ctrl.clj";
          useSourcePath = true;
        });
      in
      {
        user.packages = with pkgs; [
          pkg
        ];
      }
    );
}

{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.bdocs;
in
{
  options.modules-new.scripts.bdocs = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "bdocs" {
          content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/bdocs.clj";
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

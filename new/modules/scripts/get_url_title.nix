{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.get_url_title;
in
{
  options.modules-new.scripts.get_url_title = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "get_url_title" {
          content = ./src/get_url_title.clj;
        });
      in
      {
        user.packages = with pkgs; [
          pkg
        ];
      }
    );
}

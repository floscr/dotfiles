{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.watch_last;
in
{
  options.modules-new.scripts.watch_last = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "watch_last" {
          content = ./src/watch_last.clj;
        });
      in
      {
        user.packages = with pkgs; [
          pkg
          # go
        ];
        modules.bindings.items = [
          {
            description = "Reopen last watched";
            command = "watch_last";
          }
        ];
      }

    );
}

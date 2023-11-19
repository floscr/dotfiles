{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.bhotplug;
in
{
  options.modules-new.scripts.bhotplug = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "bhotplug" {
          content = ./src/bhotplug.clj;
        });
      in
      {
        systemd.user.services.bhotplug = {
          description = "Load my monitor modifications";
          path = with pkgs; [
            coreutils
            gnugrep
            systemd
            xorg.xrandr
            xorg.xsetroot
            xorg.xrdb
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = "${pkgs.user.babashka}/bin/bb /home/floscr/.config/dotfiles/new/modules/scripts/src/bhotplug.clj";
          };
          wantedBy = [ "default.target" ];
        };
      }
    );
}

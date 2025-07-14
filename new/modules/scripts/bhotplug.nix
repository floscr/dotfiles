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
            babashka
            coreutils
            systemd
            xorg.xrandr
            xorg.xsetroot
            xorg.xrdb
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = "${pkgs.babashka}/bin/bb --config %h/.config/dotfiles/new/modules/scripts/bb.edn %h/.config/dotfiles/new/modules/scripts/src/bhotplug.clj";
          };
          wantedBy = [ "default.target" ];
        };

        modules.bindings.items = [
          {
            xmonadBinding = "<F12>";
            command = "${pkg}/bin/bhotplug";
            description = "Hotplug Monitor";
          }
        ];
      }
    );
}

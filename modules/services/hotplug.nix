{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.hotplug;
in
{
  options.modules.services.hotplug = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (
    let restartHotplugServiceCmd = "${pkgs.systemd}/bin/systemctl --user restart setup-monitor.service";
    in
    {
      nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

      systemd.user.services."hotplug-monitor@" = {
        enable = true;
        description = "Hotplug Monitor";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = false;
          ExecStart = restartHotplugServiceCmd;
        };
      };

      modules.bindings.items = [
        {
          binding = "F12";
          command = restartHotplugServiceCmd;
          description = "Hotplug Monitor";
        }
      ];

      # Jesus christ udev
      # https://superuser.com/a/1401322
      services.udev.extraRules = ''ACTION=="change", KERNEL=="card0", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", ENV{SYSTEMD_USER_WANTS}+="hotplug-monitor@$env{SEQNUM}.service", TAG+="systemd"'';

    }
  );
}

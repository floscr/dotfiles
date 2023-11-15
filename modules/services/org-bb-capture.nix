{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.services.org-bb-capture;
in
{
  options.modules.services.org-bb-capture = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    systemd.user.services.org-bb-capture = {
      description = "Org Capture Server";
      serviceConfig = {
        ExecStart = "${pkgs.user.babashka}/bin/bb --config /home/floscr/Code/Projects/org_bb_capture/projects/server/bb.edn start";
      };
      wantedBy = [ "default.target" ];
    };
  };

}

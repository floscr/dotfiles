{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.docker;
in
{
  options.modules.dev.docker = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    virtualisation = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        enableOnBoot = false;
      };
    };

    user.packages = with pkgs; [
      docker
      docker-compose
    ];

    networking.firewall.trustedInterfaces = [ "docker0" ];

    environment.shellAliases = {
      docker-killall = "docker stop $(docker ps -q)";
    };
    user.extraGroups = [ "docker" ];
  };
}

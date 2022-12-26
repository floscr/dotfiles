{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.shared.sudoers;
in
{
  options.modules.shared.sudoers = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    security.sudo.extraRules = [{
      groups = [ "wheel" ];
      commands = [
        { options = [ "NOPASSWD" ]; command = "${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -g performance"; }
        { options = [ "NOPASSWD" ]; command = "${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -g powersave"; }
        { options = [ "NOPASSWD" ]; command = "/run/current-system/sw/bin/reboot now"; }
      ];
    }];
  };
}

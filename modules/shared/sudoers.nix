{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shared.sudoers;
in
{
  options.modules.shared.sudoers = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {

    security.sudo.extraRules = [{
      groups = [ "wheel" ];
      commands = [
        { options = [ "NOPASSWD" ]; command = "${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -g performance"; }
        { options = [ "NOPASSWD" ]; command = "${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -g powersave"; }
      ];
    }];
  };
}

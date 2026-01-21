{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.compatibility;
in
{
  options.modules.shell.compatibility = {
    enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    system.activationScripts.binbash = ''
      mkdir -p /bin
      ln -sfn ${pkgs.bash}/bin/bash /bin/bash
    '';
  };
}

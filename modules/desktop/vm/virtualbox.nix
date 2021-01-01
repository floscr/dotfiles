{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.vm.virtualbox;
in {
  options.modules.desktop.vm.virtualbox = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    users.groups.vboxusers.members = [ config.user.name ];

    virtualisation.virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };

    user.extraGroups = [ "vboxusers" ];
  };
}

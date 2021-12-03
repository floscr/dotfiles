{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.vm.virtualbox;
in
{
  options.modules.desktop.vm.virtualbox = {
    enable = mkBoolOpt false;
    vagrant.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      users.groups.vboxusers.members = [ config.user.name ];

      virtualisation.virtualbox.host = {
        enable = true;
      };

      user.extraGroups = [ "vboxusers" ];
    }
    (mkIf cfg.vagrant.enable {
      environment.systemPackages = with pkgs; [
        pkgs.vagrant
      ];
      environment.shellAliases = {
        v = "vagrant";
      };
    })
  ]);
}

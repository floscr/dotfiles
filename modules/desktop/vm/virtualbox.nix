{ options, config, lib, pkgs, virtualboxPkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.vm.virtualbox;
in {
  options.modules.desktop.vm.virtualbox = {
    enable = mkBoolOpt false;
    vagrant.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      users.groups.vboxusers.members = [ config.user.name ];

      virtualisation.virtualbox.host = {
        enable = true;
        package = pkgs.virtualboxPkgs.virtualbox;
        enableExtensionPack = true;
      };

      user.extraGroups = [ "vboxusers" ];
    }
    (mkIf cfg.vagrant.enable {
      environment.systemPackages = with pkgs; [
        pkgs.virtualboxPkgs.vagrant
      ];
      environment.shellAliases = {
        v  = "vagrant";
      };
    })
  ]);
}

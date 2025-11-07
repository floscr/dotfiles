{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules-new.scripts.bblock;
in
{
  options.modules-new.scripts.bblock = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "bblock" {
          content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/bblock.clj";
          useSourcePath = true;
        });
      in
      {
        user.packages = with pkgs; [
          pkg
        ];

        # Allow root to edit hosts file
        environment.etc.hosts.mode = "0644";

        # Allow script to edit hosts file with single command
        security.sudo.extraRules = [{
          groups = [ "wheel" ];
          commands = [
            { options = [ "NOPASSWD" ]; command = "${pkg}/bin/bblock enable"; }
            { options = [ "NOPASSWD" ]; command = "${pkg}/bin/bblock disable"; }
          ];
        }];
      }
    );
}

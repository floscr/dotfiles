{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.git;
in
{
  options.modules.shell.git = {
    enable = mkBoolOpt false;
    difftastic = mkOption {
      default = null;
      type = types.nullOr (types.submodule {
        options = {
          enable = mkBoolOpt false;
        };
      });
    };
  };

  config = mkIf (cfg.enable)
    (mkMerge [
      {
        user.packages = with pkgs; [
          git-lfs
          gh
          hub
          git-open
          diff-so-fancy
          (mkIf config.modules.shell.gnupg.enable
            git-crypt)
        ];

        home.configFile = {
          "git/config".source = "${configDir}/git/config";
          "git/ignore".source = "${configDir}/git/ignore";
        };

        modules.shell.zsh.rcFiles = [ "${configDir}/git/aliases.zsh" ];
      }
      (mkIf (cfg.difftastic != null && cfg.difftastic.enable) {
        user.packages = with pkgs; [
          difftastic
        ];
      })
    ]);
}

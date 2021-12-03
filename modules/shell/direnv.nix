{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.direnv;
in
{
  options.modules.shell.direnv = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.direnv ];

    home.configFile = {
      "direnv" = {
        source = "${configDir}/direnv";
        recursive = true;
      };
    };

    services.lorri.enable = true;

    modules.shell.zsh.rcInit = ''
      eval "$(direnv hook zsh)"

      # Copied from "direnv hook bash" output:
      _direnv_hook_enabled=1
      _direnv_hook() {
          if [ $_direnv_hook_enabled == "1" ]; then
              eval "$(direnv export bash)"
          fi
      };

      direnv-stop() {
          pushd $(pwd) > /dev/null
          cd
          _direnv_hook_enabled=0
          eval "$(direnv export bash)"
          popd > /dev/null
      }
      direnv-start() {
          echo "direnv: enabling shell hook"
          _direnv_hook_enabled=1
      }
    '';
  };
}

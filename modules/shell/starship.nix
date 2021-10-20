{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.starship;
in
{
  options.modules.shell.starship = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.direnv ];
    modules.shell.zsh.rcInit = ''eval "$(starship init zsh)"'';

    home-manager.users.${config.user.name}.programs.starship = {
      enable = true;
      package = pkgs.unstable.starship;
    };
  };
}

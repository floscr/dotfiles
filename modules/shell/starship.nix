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
      settings = {
        add_newline = false;
        line_break.disabled = true;
        directory = {
          truncate_to_repo = false;
          truncation_length = 5;
        };
        character = {
          success_symbol = "[λ](bold yellow)";
          error_symbol = "[λ](bold yellow)";
        };
        format = "$directory$git_branch\n$character";
      };


    };
  };
}

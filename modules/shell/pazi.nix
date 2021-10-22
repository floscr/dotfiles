{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.pazi;
in
{
  options.modules.shell.pazi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      pazi
    ];

    modules.shell.zsh =
      {
        rcInit = ''
          eval "$(pazi init zsh)"
          function zf () {
            dir="$(pazi view | cut -f 2- | fzf --tiebreak=index)"
            [[ ! -z "$dir" ]] && cd "$dir"
          }
          bindkey -s "^T" 'zf^M'
        '';
      };
  };
}

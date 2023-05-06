{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;

let
  cfg = config.modules.shell.atuin;
in
{
  options.modules.shell.atuin = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {

    user.packages = with pkgs; [
      atuin
    ];

    modules.shell.zsh =
      {
        rcInit = ''
          export ATUIN_NOBIND="true"
          eval "$(atuin init zsh --disable-up-arrow)"

          bindkey '^r' _atuin_search_widget
        '';
      };

  };
}

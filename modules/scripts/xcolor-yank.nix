{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.scripts.xcolor-yank;
in
{
  options.modules.scripts.xcolor-yank = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (
        let
          xcolor = "${pkgs.xcolor}/bin/xcolor";
          xclip = "${pkgs.xclip}/bin/xclip";
        in
        pkgs.writeScriptBin "xcolor-yank" ''
          #!${stdenv.shell}
          ${xcolor} | tr -d '\n' | ${xclip} -selection clipboard -in
        ''
      )
    ];
  };
}

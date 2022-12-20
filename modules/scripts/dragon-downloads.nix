{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.scripts.dragon-downloads;
in
{
  options.modules.scripts.dragon-downloads = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (pkgs.writeScriptBin "dragon_downloads" ''
        #!${stdenv.shell}
        cd ~/Downloads
        ls -t | head -n 15 | xargs -d '\n' bash -c '${xdragon}/bin/dragon --and-exit "$@"' _
      '')
    ];
  };
}

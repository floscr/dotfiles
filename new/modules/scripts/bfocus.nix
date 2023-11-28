{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules-new.scripts.bfocus;
in
{
  options.modules-new.scripts.bfocus = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable (
    let
      port = 42069;
      pkg = (pkgs.writeBb "bfocus" {
        content = ./src/bfocus.clj;
      });
    in
    {
      user.packages = with pkgs; [
        pkg
        # Lightweight alternative to query focus time
        (writeScriptBin "bfocus-time" ''
          #!${stdenv.shell}
          ${wget}/bin/wget --method=GET 'http://localhost:${port}/print-current-timer' -qO-
          exit 0
        '')
      ];
    }
  );
}

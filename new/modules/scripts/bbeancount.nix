{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.bbeancount;
in
{
  options.modules-new.scripts.bbeancount = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      pkg
      unpaper
      tesseractg
      poppler-utils # pdfunite
    ];
  };
}

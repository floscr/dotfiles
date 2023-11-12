{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.bscan;
in
{
  options.modules-new.scripts.bscan = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "bscan" {
          content = ./src/bscan.clj;
        });
      in
      {
        user.packages = with pkgs; [
          pkg
          unpaper
          tesseract
          poppler_utils # pdfunite
        ];
      }
    );
}

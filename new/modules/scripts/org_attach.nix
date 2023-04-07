{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.org_attach;
in
{
  options.modules-new.scripts.org_attach = with my; {
    enable = mkBoolOpt false;
  };

  config =
    let
      org_attach = (pkgs.writeBb "org_attach" {
        content = ./src/org_attach.clj;
      });
    in
    {
      user.packages = with pkgs; [
        xclip
        org_attach
      ];
    };
}

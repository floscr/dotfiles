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
        content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/org_attach.clj";
        useSourcePath = true;
      });
    in
    {
      user.packages = with pkgs; [
        xclip
        org_attach
      ];
    };
}

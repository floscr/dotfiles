{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.invert_colors;
in
{
  options.modules-new.scripts.invert_colors = with my; {
    enable = mkBoolOpt false;
  };

  config =
    let
      invert_colors = (pkgs.writeBb "invert_colors" {
        content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/invert_colors.clj";
        useSourcePath = true;
      });
    in
    {
      user.packages = with pkgs;
        [
          invert_colors
        ];
      modules.bindings.items = [
        {
          xmonadBinding = "M-i";
          command = "${invert_colors}/bin/invert_colors";
          description = "Invert Colors";
        }
      ];
    };
}

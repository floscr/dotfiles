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
      pkg = (pkgs.writeBb "invert_colors" {
        content = ./invert_colors.clj;
      });
    in
    {
      user.packages = with pkgs; [
        pkg
      ];
      modules.bindings.items = [
        {
          xmonadBinding = "M-i";
          command = "${pkg}/bin/invert_colors";
          description = "Invert Colors";
        }
      ];

    };
}

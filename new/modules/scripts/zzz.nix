{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.zzz;
in
{
  options.modules-new.scripts.zzz = with my; {
    enable = mkBoolOpt false;
  };

  config =
    let
      pkg = (pkgs.writeBb "zzz" {
        content = ./src/zzz.clj;
        deps = with pkgs; [
          dunst
        ];
      });
    in
    {
      user.packages = with pkgs; [
        pkg
      ];
      modules.bindings.items = [
        {
          xmonadBinding = "M-<Backspace>";
          command = "${pkg}/bin/zzz";
          description = "Turn off display (zzz)";
        }
        {
          xmonadBinding = "M-S-<Backspace>";
          command = "${pkg}/bin/zzz sleep";
          description = "Sleep (zzz)";
        }
      ];
    };
}

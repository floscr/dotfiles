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
      script-name = "zzz";
      package = (writeBabashkaScriptBin script-name ./zzz.clj);
      bin = "${package}/bin/${script-name}";
      dir = cfg.plugins.record.dir;
    in
    {
      user.packages = with pkgs; [
        package
        dunst
      ];
      modules.bindings.items = [
        {
          xmonadBinding = "M-<Backspace>";
          command = "zzz";
          description = "Turn off display (zzz)";
        }
        {
          xmonadBinding = "M-S-<Backspace>";
          command = "zzz sleep";
          description = "Sleep (zzz)";
        }
      ];
    };
}

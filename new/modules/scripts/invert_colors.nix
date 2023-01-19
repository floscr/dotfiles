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
      script-name = "invert_colors";
      package = (writeBabashkaScriptBin script-name ./invert_colors.clj);
      bin = "${package}/bin/${script-name}";
    in
    {
      user.packages = with pkgs; [
        package
      ];
      modules.bindings.items = [
        {
          xmonadBinding = "M-i";
          command = "${bin}";
          description = "Invert Colors";
        }
      ];

    };
}

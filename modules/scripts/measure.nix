{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.scripts.measure;
  script-name = "measure";
in
{
  options.modules.scripts.measure = {
    enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [
      maim
      bc
      (pkgs.writeScriptBin script-name (builtins.readFile ./measure.sh))
    ];

    modules.bindings.items = [
      {
        description = "Ruler (Measure)";
        command = "${script-name}";
      }
    ];
  };
}

{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.scripts.measure;
  script-name = "measure";
in
{
  options.modules.scripts.measure = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
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

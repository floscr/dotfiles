{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.brave;
in
{
  options.modules.desktop.browsers.brave = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      brave
    ];
    modules.bindings.items = [
      {
        description = "Brave";
        command = "brave";
      }
      {
        description = "Brave (Private)";
        command = "brave --incognito";
      }
    ];
  };
}

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.work.pitch;
in
{
  options.modules.work.pitch = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.enable) (mkMerge [
    {
      user.packages = with pkgs; [
        zoom-us
        nodePackages.mermaid-cli # Mermaid Diagrams CLI, mmdc
      ];
    }
  ]);
}

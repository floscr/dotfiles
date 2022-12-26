{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.scripts;
in
{
  options.modules.scripts.default = { };

  config = mkIf cfg.enable { };
}

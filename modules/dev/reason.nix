{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.reason;
in
{
  options.modules.dev.reason = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ocamlPackages.reason
      ocamlPackages.merlin
    ];
  };
}

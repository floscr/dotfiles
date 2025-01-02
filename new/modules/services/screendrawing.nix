{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.screendrawing;
in
{
  options.modules.services.screendrawing = {
    enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [
      gromit-mpx
    ];

    modules.bindings.items = [
      {
        description = "Start Screendrawing";
        binding = "F9";
        command = "gromit-mpx -a";
      }
    ];
  };
}

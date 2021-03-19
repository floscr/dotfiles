{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.gaming.isaac;
in {
  options.modules.desktop.gaming.isaac = {
    enable = mkBoolOpt false;
  };

  config = {
    modules.bindings.items = [
      {
        description = "Binding of Isaac";
        command = "rm -rf ~/.local/share/Steam/bootstrap.tar.xz; steam steam://rungameid/250900";
      }
    ];
  };
}

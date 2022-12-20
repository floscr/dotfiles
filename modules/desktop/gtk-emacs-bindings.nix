{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.gtk-emacs-bindings;
in
{
  options.modules.desktop.gtk-emacs-bindings = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { };
}

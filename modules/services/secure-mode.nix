{ config, options, lib, home-manager, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.secure-mode-scripts;
in
{
  options.modules.services.secure-mode-scripts = with types; {
    enable = mkBoolOpt false;
    items = mkOption {
      type = listOf (submodule ({ name, ... }: {
        options.onEnable = mkOption { type = str; };
        options.onDisable = mkOption { type = str; };
      }));
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    user.packages =
      let
        xs = config.modules.services.secure-mode-scripts.items;
        enableScripts = lib.concatMapStrings (x: x.onEnable + "\n") xs;
        disableScripts = lib.concatMapStrings (x: x.onDisable + "\n") xs;
      in
      with pkgs; [
        (writeScriptBin "secure-mode-enable" ''
          #!${stdenv.shell}
          ${enableScripts}
        '')
        (writeScriptBin "secure-mode-disable" ''
          #!${stdenv.shell}
          ${disableScripts}
        '')
      ];
  };
}

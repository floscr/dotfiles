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

  config = mkIf cfg.enable (
    let
      xs = config.modules.services.secure-mode-scripts.items;
      enableScripts = lib.concatMapStrings (x: x.onEnable + "\n") xs;
      enableBin = (pkgs.writeScriptBin "secure-mode-enable" ''
        #!${pkgs.stdenv.shell}
        ${enableScripts}
      '');
      disableScripts = lib.concatMapStrings (x: x.onDisable + "\n") xs;
      disableBin = (pkgs.writeScriptBin "secure-mode-disable" ''
        #!${pkgs.stdenv.shell}
        ${disableScripts}
      '');
    in
    {
      user.packages = with pkgs; [
        enableBin
        disableBin
      ];
      modules.bindings.items = [
        {
          command = "${enableBin}/bin/secure-mode-enable";
          description = "Secure Mode: Enable";
        }
        {
          command = "${disableBin}/bin/secure-mode-disable";
          description = "Secure Mode: Disable";
        }
      ];
    }
  );
}

{ config, options, lib, home-manager, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.secure-mode-scripts;
in
{
  options.modules.services.secure-mode-scripts = with types; {
    enable = mkBoolOpt false;
    tmpFile = mkStrOpt "/tmp/secure-mode-enabled";
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
        echo "Secure Mode: Enabled"
        touch ${cfg.tmpFile}
        ${enableScripts}
      '');
      enableScript = "${enableBin}/bin/secure-mode-enable";

      disableScripts = lib.concatMapStrings (x: x.onDisable + "\n") xs;
      disableBin = (pkgs.writeScriptBin "secure-mode-disable" ''
        #!${pkgs.stdenv.shell}
        echo "Secure Mode: Disabled"
        rm -rf ${cfg.tmpFile}
        ${disableScripts}
      '');
      disableScript = "${disableBin}/bin/secure-mode-disable";

      toggleBin = (pkgs.writeScriptBin "secure-mode-toggle" ''
        #!${pkgs.stdenv.shell}
        FILE=${cfg.tmpFile}
        if [[ -f "$FILE" ]]; then
            ${disableScript}
        else
            ${enableScript}
        fi
        ${disableScripts}
      '');
      toggleScript = "${toggleBin}/bin/secure-mode-toggle";
    in
    {
      user.packages = with pkgs; [
        enableBin
        disableBin
        toggleBin
      ];
      modules.bindings.items = [
        {
          command = toggleScript;
          description = "Secure Mode: Toggle";
        }
        {
          command = enableScript;
          description = "Secure Mode: Enable";
        }
        {
          command = disableScript;
          description = "Secure Mode: Disable";
        }
      ];
    }
  );
}

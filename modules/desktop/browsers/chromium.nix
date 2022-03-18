{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.chromium;
in
{
  options.modules.desktop.browsers.chromium = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      chromium
      (pkgs.writeScriptBin "launch-chrome" ''
        #! ${pkgs.bash}/bin/bash
        # start chromium depending on which display is connected

        export DEFAULT_ARGS="--enable-native-notifications --restore-last-session"

        if [[ $(xrandr | grep "^eDP-1 connected primary") ]]; then
          chromium-browser $DEFAULT_ARGS --force-device-scale-factor=1.2 $@
        else
          chromium-browser $DEFAULT_ARGS --force-device-scale-factor=1.5 $@
        fi
      '')
      (pkgs.writeScriptBin "chromium-private" ''
        #! ${pkgs.bash}/bin/bash
        launch-chrome --incognito "$@"
      '')
    ];

    # Needed for netflix
    nixpkgs.config.chromium = {
      enableWideVine = true;
    };

    modules.bindings.items = [
      {
        description = "Chrome";
        categories = "Script";
        command = "launch-chrome";
      }
      {
        description = "Chrome (Private)";
        categories = "Script";
        command = "launch-chrome --incognito";
      }
    ];
  };
}

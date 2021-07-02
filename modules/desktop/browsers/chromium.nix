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

    # Extensions
    programs.chromium = {
      enable = true;
      defaultSearchProviderSearchURL = "https://duckduckgo.com/?q={searchTerms}";
      defaultSearchProviderSuggestURL = "https://ac.duckduckgo.com/ac/?q={searchTerms}&type=list";
      extensions = [
        "icpgjfneehieebagbmdbhnlpiopdcmna" # Add URL to window title
        "gcbommkclmclpchllfjekcdonpmejbdp" # Atomic Chrome, edit inputs in emacs
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # Dark Reader
        "dbepggeogbaibhgnhhndojpepiihcmeb" # Georgify - Hacker News Theme
        "lhaoghhllmiaaagaffababmkdllgfcmc" # HTTPS Everywhere
        "eimadpbcbfnmbkopoojfekhnkhdbieeh" # JSON Viewer
        "kbmfpngjjgdllneeigpgjifpgocmfgmb" # New Tab URL - To fix vimium new tab disable
        "ofjfdfaleomlfanfehgblppafkijjhmi" # Reddit Enhancment Suite
        "gbmdgpbipfallnflgajpaliibnhdgobh" # Tab Shortcuts
        "ndiaggkadcioihmhghipjmgfeamgjeoi" # Vimium
        "fipfgiejfpcdacpjepkohdlnjonchnal" # uBlock Origin
      ];
    };
  };
}

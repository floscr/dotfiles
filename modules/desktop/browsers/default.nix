{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers;
in
{
  options.modules.desktop.browsers = {
    default = mkOpt (with types; nullOr str) null;
    psd = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Enable Profile Sync Daemon (PSD) for browser profiles.
        PSD syncs browser profile directories to tmpfs (RAM) and periodically backs them up to disk.
        
        - Improved browser performance (profile data in RAM)
        - Reduced disk wear (especially for SSDs)
      '';
    };
    customOpener = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Path to a custom browser opener script that will be used instead of the default browser.
        This script will be set as the BROWSER environment variable and used by xdg-open.
        The script should accept a URL as the first argument.
        
        If null, the default browser will be used directly.
      '';
    };
  };

  config = mkIf (cfg.default != null) {
    services.psd.enable = cfg.psd;

    # Use custom browser opener if set, otherwise use default browser
    env.BROWSER =
      if cfg.customOpener != null
      then cfg.customOpener
      else cfg.default;

    # When a custom opener is set, create browser-launcher.desktop so
    # xdg-open routes through it (mimeapps.list references this name).
    user.packages = mkIf (cfg.customOpener != null) [
      (pkgs.makeDesktopItem {
        name = "browser-launcher";
        desktopName = "Browser Launcher";
        genericName = "Web Browser";
        exec = "${cfg.customOpener} %U";
        icon = "web-browser";
        categories = [ "Network" "WebBrowser" ];
        mimeTypes = [
          "text/html"
          "text/xml"
          "application/xhtml+xml"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/about"
          "x-scheme-handler/unknown"
          "x-scheme-handler/mailto"
          "image/svg+xml"
        ];
      })
    ];

    home.configFile = {
      "browser/home.html".text = ''
        <html>
          <head>
            <style type="text/css" media="screen">
              html {
                  background-color: ${config.modules.theme.colors.bg0};
              }
            </style>
          </head>
        </html>
      '';
    };

  };
}

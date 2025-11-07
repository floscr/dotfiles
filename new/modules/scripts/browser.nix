{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules-new.scripts.browser;
in
{
  options.modules-new.scripts.browser = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable (
    let
      browserScript = pkgs.writeBb "browser" {
        content = ./src/browser.clj;
      };
    in
    {
      user.packages = with pkgs; [
        browserScript
        (makeDesktopItem {
          name = "browser-launcher";
          desktopName = "Browser Launcher";
          genericName = "Dynamic Browser Selector";
          icon = "web-browser";
          exec = "${browserScript}/bin/browser %U";
          categories = [ "Network" "WebBrowser" ];
          mimeTypes = [
            "text/html"
            "text/xml"
            "application/xhtml+xml"
            "x-scheme-handler/http"
            "x-scheme-handler/https"
            "x-scheme-handler/about"
            "x-scheme-handler/unknown"
          ];
        })
      ];
    }
  );
}

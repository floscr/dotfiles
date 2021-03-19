{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.mime;
in {
  options.modules.desktop.mime = with types; {
    enable = mkBoolOpt false;
    browser = mkStrOpt "chromium-browser.desktop";
    images = mkStrOpt "feh.desktop";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.xdg.mimeApps = let
      browser = [ cfg.browser ];
      images = [ cfg.images ];
    in {
      enable = true;
      defaultApplications = {
        "application/xhtml+xml" = browser;
        "text/html" = browser;
        "text/xml" = browser;
        "x-scheme-handler/about" = browser;
        "x-scheme-handler/unknown" = browser;
        "x-scheme-handler/mailto" = browser;
        "x-scheme-handler/http" = browser;
        "x-scheme-handler/https" = browser;
        "image/svg+xml" = browser;

        "image/gif" = images;
        "image/jpeg" = images;
        "image/png" = images;
      };
    };
  };
}

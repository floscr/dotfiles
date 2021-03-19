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
    home-manager.users.${config.user.name}.xdg.mimeApps.defaultApplications = {
      "application/xhtml+xml" = cfg.browser;
      "text/html" = cfg.browser;
      "text/xml" = cfg.chromium;
      "x-scheme-handler/http" = cfg.chromium;
      "x-scheme-handler/https" = cfg.chromium;
      "image/svg+xml" = cfg.chromium;

      "image/gif" = cfg.images;
      "image/jpeg" = cfg.images;
      "image/png" = cfg.images;
    };
  };
}

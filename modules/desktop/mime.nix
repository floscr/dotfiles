{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.mime;
in
{
  options.modules.desktop.mime = with types; {
    enable = mkBoolOpt false;
    browser = mkStrOpt "browser-launcher.desktop";
    directories = mkStrOpt "my-emacsclient.desktop";
    images = mkStrOpt "feh.desktop";
    text = mkStrOpt "my-emacsclient.desktop";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.xdg.mimeApps =
      let
        browser = [ cfg.browser ];
        directories = [ cfg.directories ];
        images = [ cfg.images ];
        text = [ cfg.text ];
      in
      {
        enable = true;
        defaultApplications = {
          "application/x-directory" = directories;
          "inode/directory" = directories;

          "text/english" = text;
          "text/plain" = text;
          "text/x-c" = text;
          "text/x-c++" = text;
          "text/x-c++hdr" = text;
          "text/x-c++src" = text;
          "text/x-chdr" = text;
          "text/x-csrc" = text;
          "text/x-java" = text;
          "text/x-makefile" = text;
          "text/x-moc" = text;
          "text/x-pascal" = text;
          "text/x-tcl" = text;
          "text/x-tex" = text;

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

{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.desktop.apps.zoom-noop;
  noop = (pkgs.writeScriptBin "noop" ''
    #!${pkgs.stdenv.shell}
    echo "Noop for: " $@
  '');
in
{
  options.modules.desktop.apps.zoom-noop = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home = {
      defaultApplications = {
        "x-scheme-handler/zoommtg" = "zoom-noop-protocol";
      };
      desktopEntries."zoom-noop-protocol" = {
        name = "Zoom Noop";
        comment = "Fake zoom protocol to prevent zoom in the browser opening a zoom client";
        exec = "${noop}/bin/noop";
        mimeType = [ "x-scheme-handler/zoommtg" ];
      };
    };
  };
}

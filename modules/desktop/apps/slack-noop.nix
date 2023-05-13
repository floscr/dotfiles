{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.desktop.apps.slack-noop;
  noop = (pkgs.writeScriptBin "noop" ''
    #!${pkgs.stdenv.shell}
    echo "Noop for: " $@
  '');
in
{
  options.modules.desktop.apps.slack-noop = {
    enable = my.mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home = {
      defaultApplications = {
        "x-scheme-handler/slack" = "slack-noop-protocol";
      };
      desktopEntries."slack-noop-protocol" = {
        name = "Slack Noop";
        comment = "Fake slack protocol to prevent slack in the browser opening a slack client";
        exec = "${noop}/bin/noop";
        mimeType = [ "x-scheme-handler/slack" ];
      };
    };
  };
}

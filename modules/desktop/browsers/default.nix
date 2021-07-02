{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers;
in
{
  options.modules.desktop.browsers = {
    default = mkOpt (with types; nullOr str) null;
    psd = mkBoolOpt false;
  };

  config = mkIf (cfg.default != null) {
    services.psd.enable = true;
    env.BROWSER = cfg.default;
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

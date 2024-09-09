{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.playwright;
in
{
  options.modules.dev.playwright = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      playwright-driver.browsers
    ];
    env = {
      PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
      PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
    };
  };
}

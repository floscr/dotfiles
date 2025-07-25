{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.bplaywright;
in
{
  options.modules-new.scripts.bplaywright = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "bplaywright" {
          content = ./src/bplaywright.clj;
          env = {
            PLAYWRIGHT_BROWSERS_PATH = pkgs.playwrightPkgs.playwright-driver.browsers;
          };
        });
      in
      {
        user.packages = with pkgs; [
          playwrightPkgs.python313Packages.playwright
          playwrightPkgs.playwright-driver.browsers
          pkg
        ];
        env = {
          PLAYWRIGHT_BROWSERS_PATH = pkgs.playwrightPkgs.playwright-driver.browsers;
          PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
          PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true";
        };
      }
    );
}

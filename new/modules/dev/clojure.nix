{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.clojure;
in
{
  options.modules.dev.clojure = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # Clojure
      clojure
      openjdk17

      # Editing
      unstable.clojure-lsp

      # Babashka
      babashka
      user.bootleg # HTML Conversion
      gum

      # Formatting
      clj-kondo
    ];

    env = {
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    };
  };
}

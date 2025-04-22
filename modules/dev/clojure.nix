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

      # Clojure CLIs
      jet # JSON/EDN Query

      # Formatting
      clj-kondo
      cljfmt
      zprint

      # uix
      unstable.lightningcss
    ];
    # home.configFile."clojure/deps.edn".source = ./deps.edn;
  };
}

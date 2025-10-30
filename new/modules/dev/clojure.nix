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

    # Trying to move clojure/java cache files, but it's mostly useless as its configured in multiple places
    env = {
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      GITLIBS = "$XDG_CACHE_HOME/gitlibs";

      MAVEN_USER_HOME = "$XDG_CACHE_HOME/maven";
      MAVEN_CONFIG = "$XDG_CONFIG_HOME/maven";
      MAVEN_OPTS = "-Dmaven.repo.local=$XDG_CACHE_HOME/maven/repository";
      BABASHKA_CLASSPATH_CACHE = "$XDG_CACHE_HOME/babashka";
    };
  };
}

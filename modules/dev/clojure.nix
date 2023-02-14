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
      babashka
      clj-kondo
      clojure
      jet
      joker
      leiningen
      neil
      openjdk17
      unstable.clojure-lsp
      zprint
    ];
    home.configFile."clojure/deps.edn".source = ./deps.edn;
  };
}

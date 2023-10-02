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
      # wrapped babahska doesn't let me start bb repl anymore
      # Changed here: https://github.com/NixOS/nixpkgs/pull/241119
      babashka-unwrapped
      unstable.lightningcss # uix
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

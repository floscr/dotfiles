{ lib, pkgs, ... }:

with builtins;
with lib;
{
  writeBabashkaScriptBin = script-name: path: env:
    (pkgs.writeScriptBin
      script-name
      ("${env} "
        + "${pkgs.babashka}/bin/bb "
        + "--deps-root $HOME/.config/dotfiles/new/modules/scripts "
        + "--config $HOME/.config/dotfiles/new/modules/scripts/bb.edn "
        + "${path} "
        + "$@"));
}

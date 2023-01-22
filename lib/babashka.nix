{ lib, pkgs, ... }:

with builtins;
with lib;
{
  writeBabashkaScriptBin = script-name: path:
    (pkgs.writeScriptBin
      script-name
      ("${pkgs.babashka}/bin/bb "
        + "--deps-root $HOME/.config/dotfiles/new/modules/scripts "
        + "--config $HOME/.config/dotfiles/new/modules/scripts/bb.edn "
        + "${path} "
        + "$@"));
}

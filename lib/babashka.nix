{ lib, pkgs, ... }:

with builtins;
with lib;
{
  writeBabashkaScriptBin = script-name: path:
    (pkgs.writeScriptBin
      script-name
      ("${pkgs.babashka}/bin/bb "
        # Fixes execution path when in a repository with another bb.edn file
        + "--deps-root ${../.} --classpath ${../.} "
        + "${path} "
        + "$@"));
}

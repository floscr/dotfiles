{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  user.packages = with pkgs; [
    zoom-us
    nodePackages.mermaid-cli # Mermaid Diagrams CLI, mmdc
    epiphany
  ];

  modules = {
    dev = {
      clojure.enable = true;
    };
  };
}

{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  user.packages = with pkgs; [
    zoom-us
    nodePackages.mermaid-cli # Mermaid Diagrams CLI, mmdc
  ];

  modules = {
    dev = {
      clojure.enable = true;
    };
    desktop.apps = {
      zoom.enable = true;
    };
  };

  modules.bindings.items = [
    {
      description = "Epiphany (Webkit)";
      command = "${pkgs.epiphany}/bin/epiphany";
    }
  ];
}

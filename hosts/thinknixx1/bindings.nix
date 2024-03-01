{ config, lib, pkgs, ... }:

{
  modules.bindings.items = [
    {
      description = "Dir: ~";
      command = "bb /home/floscr/Code/Projects/iced-prompt/scripts/src/file_explorer.clj ~";
      action = "next";
    }
  ];
}

{ config, lib, pkgs, ... }:

{
  user.packages = with pkgs; [
    (writeBb "connection-toggle" {
      content = ./test.clj;
      deps = [ rofi networkmanager ];
    })
  ];
}

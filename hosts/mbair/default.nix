{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
  ];
  modules = {
    shell = {
      direnv.enable = true;
      git.enable    = true;
      gnupg.enable  = true;
      pass.enable   = true;
      zsh.enable    = true;
    };
  };

  networking.useDHCP = false;

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  
  environment.systemPackages = with pkgs; [
    firefox
  ];
}

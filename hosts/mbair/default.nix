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
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      keyboard.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
      };
      sensors.enable = true;
    };
    desktop = {
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      term = {
        default = "termite";
        termite.enable = true;
      };
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      vim.enable = true;
    };
  };

  networking.useDHCP = false;

  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
    windowManager.bspwm.enable = true;

    # Configure keymap in X11
    layout = "us";
    xkbOptions = "eurosign:e";

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;
  };

  networking.networkmanager.enable = true;
}

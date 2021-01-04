{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
    ../personal.nix
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
    services = {
      syncthing.enable = true;
    };
    theme = {
      colorscheme = "gruvbox-dark";
      vimColorscheme = "gruvbox";
      batTheme = "gruvbox";
      gitDeltaTheme = "gruvbox";

      fonts = rec {
        sans = {
          family = "Source Sans Pro";
          size = 8;
          pkg = pkgs.source-sans-pro;
        };
        serif = {
          family = "Source Serif Pro";
          size = 8;
          pkg = pkgs.source-serif-pro;
        };
        mono = {
          family = "Iosevka";
          size = 8;
          pkg = pkgs.iosevka;
        };
        ui = sans;
      };
    };
  };

  home-manager.users.${config.user.name}.programs.termite.font = "${config.modules.theme.primaryMonospaceFont} 12";

  networking.useDHCP = false;

  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
    windowManager.bspwm.enable = true;

    # Fix the horrible color profile on the display from linux
    # Export your default color profile from the mac osx partition
    # Source: https://github.com/willtim/nixos/blob/52e730ec0d8288a3862538205cd8ff0fa2d1c159/desktop.nix#L151
    # xiccd apparently is buggy and cpu intensive
    displayManager.sessionCommands = ''
      {pkgs.argyllcms}/bin/dispwin -I "~/.local/macbook-air-lcd.icc"
    '';

    # Configure keymap in X11
    layout = "us";
    xkbOptions = "eurosign:e";

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;
  };
  
}

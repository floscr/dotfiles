{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  imports = [
    ./sound.nix
    ./throttled.nix
    ./hardware-configuration.nix
    ../personal.nix
  ];

  user.packages = with pkgs; [
    flameshot
    gparted
    jq
    screenkey
    weechat
    signal-desktop
    unrar
    unzip
    blender
    vlc
    appimage-run
    user.scdl
  ];

  hardware.usb.wakeupDisabled = [
    {
      # Logitech, Inc. G3 (MX518) Optical Mouse
      vendor = "046d";
      product = "c051";
    }
  ];

  modules = {
    bindings.enable = true;
    shell = {
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      pass.enable = true;
      zsh.enable = true;
      youtube-dl.enable = true;
      color-optimization.enable = true;
    };
    shared = {
      sudoers.enable = true;
    };
    hardware = {
      trackpad.enable = true;
      audio.enable = true;
      bluetooth.enable = true;
      keyboard.enable = true;
      external-display.enable = true;
      scanner.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
        autoMount.enable = true;
      };
      sensors.enable = true;
    };
    dev = {
      node.enable = true;
      nim.enable = true;
      docker.enable = true;
      mysql.enable = false;
    };
    desktop = {
      bspwm.enable = true;
      mime.enable = true;
      gtk-emacs-bindings.enable = true;
      vm = {
        virtualbox = {
          enable = true;
          vagrant.enable = true;
        };
      };
      apps = {
        rofi.enable = true;
        transmission.enable = true;
      };
      gaming = {
        steam.enable = true;
        isaac.enable = true;
      };
      graphics = {
        gimp.enable = true;
        inkscape.enable = true;
      };
      media = {
        spotify.enable = true;
        mpv.enable = true;
      };
      browsers = {
        default = "chromium";
        psd = true;
        chromium.enable = true;
        firefox.enable = true;
        brave.enable = true;
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
      polybar.enable = true;
      dunst.enable = true;
      greenclip.enable = true;
      hotplug.enable = true;
    };
    work.meisterlabs.enable = true;
    theme.active = "opera";
  };

  console = {
    font =  "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Automatically log in since the disk is encrypted with a password anyway
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = config.user.name;

  # Monitor backlight control
  programs.light.enable = true;
  user.extraGroups = [ "video" ];
}

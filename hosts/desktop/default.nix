{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
    ../personal.nix
  ];

  # Disable pipewire (conflicts with pulseaudio from hardware.audio module)
  services.pipewire.enable = lib.mkForce false;

  networking.firewall = {
    allowedTCPPorts = [
      9630
      1280
      1290
    ];
    allowedUDPPorts = [ ];
  };

  user.packages = with pkgs; [

  ] ++ flake-packages;

  hardware.logitech.wireless.enable = true;
  hardware.logitech.wireless.enableGraphical = true;

  hardware.usb.wakeupDisabled = [
    {
      # Logitech, Inc. G3 (MX518) Optical Mouse
      vendor = "046d";
      product = "c051";
    }
    {
      # Logitech Mx Master USB Receiver
      vendor = "046d";
      product = "c548";
    }
  ];

  services.atd.enable = true;

  modules-new = {
    scripts = {
      shell_history.enable = true;
      bblock.enable = true;
      bbluetooth.enable = true;
      bdocs.enable = true;
      bfocus.enable = true;
      bhotplug.enable = true;
      bplaywright.enable = true;
      bscan.enable = true;
      get_url_title.enable = true;
      invert_colors.enable = true;
      monitor_brightness.enable = true;
      mpv_ctrl.enable = true;
      org_attach.enable = true;
      screen-capture.enable = true;
      watch_last.enable = true;
      zzz.enable = true;
    };
  };

  modules = {
    bindings.enable = true;
    shell = {
      atuin.enable = true;
      tmux.enable = true;
      bat.enable = true;
      sysz.enable = true;
      direnv.enable = true;
      git.enable = true;
      git.difftastic.enable = true;
      gnupg.enable = true;
      pass.enable = true;
      pazi.enable = true;
      starship.enable = true;
      youtube-dl.enable = true;
      zsh.enable = true;
      wget.enable = true;
      beancount.enable = true;
    };
    shared = {
      sudoers.enable = true;
    };
    hardware = {
      printer.brother.hl1210w.enable = true;
      webcam.logitech-c920.enable = true;
      audio.enable = true;
      bluetooth.enable = true;
      keyboard.enable = true;
      scanner.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
        autoMount.enable = true;
      };
      sensors.enable = true;
    };
    dev = {
      docker.enable = true;
      playwright.enable = false;
      tools.enable = true;
      godot.enable = false;
      node.enable = true;
      nix.enable = true;
      clojure.enable = true;
      rust.enable = true;
      ai.enable = true;
      ai.antigravity.enable = true;
      ai.gemini.enable = true;
    };
    desktop = {
      xmonad.enable = true;
      mime.enable = true;
      gtk-emacs-bindings.enable = true;
      gtk.enable = true;
      apps = {
        rofi.enable = true;
        transmission.enable = true;
        font-manager.enable = true;
        zoom-noop.enable = true;
        slack-noop.enable = true;
      };
      gaming = {
        steam.enable = true;
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
        default = "firefox";
        customOpener = "browser_open";
        chromium.enable = true;
        firefox.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
    };
    editors = {
      default = "nvim";
      emacs = {
        enable = true;
        enableMail = false;
      };
      vim.enable = true;
    };
    services = {
      ios.enable = false;
      org-bb-capture.enable = true;
      wireguard.enable = false;
      android.enable = true;
      screendrawing.enable = true;
      syncthing.enable = true;
      dunst.enable = true;
      greenclip.enable = true;
      hotplug.enable = true;
      flameshot.enable = true;
      network-monitoring.enable = true;
      picom.enable = true;
      opensnitch.enable = false;
      kdeconnect.enable = true;
    };
    theme.active = "opera";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Automatically log in since the disk is encrypted with a password anyway
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = config.user.name;

  # Gamemode for better gaming performance
  programs.gamemode.enable = true;

  user.extraGroups = [ "video" "dialout" ];
}

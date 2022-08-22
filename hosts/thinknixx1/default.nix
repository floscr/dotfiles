{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
    ./sound.nix
    # ./throttled.nix
    ../personal.nix
    ../pitch.nix
    # ./bindings.nix
  ];

  home.file = {
    ".config/espanso/user/email.yml".source = config.age.secrets.espanso-private-emails.path;
  };

  user.packages = with pkgs; [
    calibre
    unzip
    s-tui
    custom.nim-utils.get_url_title
    custom.nim-utils.bose_battery_level
    signal-desktop
    jellyfin-media-player
    lsof
  ] ++ flake-packages;

  hardware.usb.wakeupDisabled = [
    {
      # Logitech, Inc. G3 (MX518) Optical Mouse
      vendor = "046d";
      product = "c051";
    }
  ];

  services.atd.enable = true;

  modules = {
    bindings.enable = true;
    shell = {
      bat.enable = true;
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
    scripts = {
      screen-record.enable = true;
      measure.enable = true;
      comma.enable = true;
    };
    shared = {
      sudoers.enable = true;
    };
    hardware = {
      webcam.logitech-c920.enable = true;
      trackpad.enable = true;
      audio.enable = true;
      bluetooth.enable = true;
      keyboard.enable = true;
      external-display.enable = true;
      scanner.enable = true;
      battery.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
        autoMount.enable = true;
      };
      sensors.enable = true;
    };
    dev = {
      tools.enable = true;
      node.enable = true;
      nim.enable = true;
      docker.enable = true;
      nix.enable = true;
    };
    desktop = {
      xmonad.enable = true;
      mime.enable = true;
      gtk-emacs-bindings.enable = true;
      # vm = {
      #   virtualbox = {
      #     enable = true;
      #     vagrant.enable = true;
      #   };
      # };
      apps = {
        rofi.enable = true;
        transmission.enable = true;
        font-manager.enable = true;
      };
      gaming = {
        steam.enable = true;
        emulators.n64.enable = true;
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
        default = "brave";
        psd = true;
        chromium.enable = true;
        firefox.enable = true;
        brave.enable = true;
        brave.profiles = [
          { dir = "Profile 1"; alias = "Pitch"; }
          { dir = "Profile 2"; alias = "Personal"; }
        ];
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
      secure-mode-scripts.enable = true;
      android.enable = true;
      espanso.enable = true;
      screendrawing.enable = true;
      syncthing.enable = true;
      polybar.enable = false;
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
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = config.user.name;

  # Monitor backlight control
  programs.light.enable = true;
  user.extraGroups = [ "video" ];

  home.configFile = {
    "gtk-3.0/bookmarks".text = ''
      file://${homeDir}/Downloads
      file://${homeDir}/Documents
    '';
  };
}

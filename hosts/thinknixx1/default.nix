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
    ../penpot.nix
    ../hyma.nix
    ./bindings.nix
  ];

  # services.openssh.enable = true;
  # programs.mosh.enable = true;


  networking.firewall = {
    allowedTCPPorts = [
      9630
      1280
      1290
    ];
    allowedUDPPorts = [
      # 51820
      # 60000
      # 60002
      # 60005
      # 65535
    ];
  };

  services.udev.extraRules = ''
    SUBSYSTEM!="usb", GOTO="end_rules"

    # RK3036
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="301a", MODE="0666", GROUP="users"
    # RK3128
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="310c", MODE="0666", GROUP="users"
    # RK3229
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="320b", MODE="0666", GROUP="users"
    # RK3288
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="320a", MODE="0666", GROUP="users"
    # RK3328
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="320c", MODE="0666", GROUP="users"
    # RK3368
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="330a", MODE="0666", GROUP="users"
    # RK3399
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="330c", MODE="0666", GROUP="users"
    # RK3566
    ATTRS{idVendor}=="2207", ATTRS{idProduct}=="350a", MODE="0666", GROUP="users"

    LABEL="end_rules"
  '';

  user.packages = with pkgs; [
    zed-editor
    textsnatcher
    calibre
    magic-wormhole
    unzip
    s-tui
    custom.nim-utils.bose_battery_level
    unstable.signal-desktop
    jellyfin-media-player
    lsof
    google-chrome
    chrysalis
    xournal
    zip
    unrar
    pins.nix-search-cli
    timer
    exiftool
    appimage-run
    usbutils
    geeqie
    watchexec
    scdl
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
      bfocus.enable = true;
      bblock.enable = true;
      bhotplug.enable = true;
      bscan.enable = true;
      bdocs.enable = true;
      screen-capture.enable = true;
      monitor_brightness.enable = true;
      zzz.enable = true;
      invert_colors.enable = true;
      get_url_title.enable = true;
      mpv_ctrl.enable = true;
      watch_last.enable = true;
      org_attach.enable = true;
      bbluetooth.enable = true;
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
      docker.enable = true;
      playwright.enable = false;
      tools.enable = true;
      godot.enable = false;
      node.enable = true;
      nix.enable = true;
      clojure.enable = true;
      rust.enable = true;
    };
    desktop = {
      xmonad.enable = true;
      mime.enable = true;
      gtk-emacs-bindings.enable = true;
      gtk.enable = true;
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
        default = "brave";
        psd = true;
        chromium.enable = true;
        firefox.enable = true;
        brave.enable = true;
        brave.profiles = [
          { dir = "Profile 2"; alias = "Personal"; }
          { dir = "Profile 3"; alias = "Johanna"; }
          { dir = "Profile 4"; alias = "Hyma"; }
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
      ios.enable = true;
      org-bb-capture.enable = true;
      wireguard.enable = false;
      android.enable = true;
      espanso.enable = false;
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
      photoprism = {
        enable = false;
        instances = {
          persons = {
            port = 2343;
            vhost = "photos.phire.org";
            storageDir = "/var/lib/photoprism-personal";
            originalsDir = "/mnt/storage/personal/originals";
            importDir = "/mnt/storage/personal/import";
          };
        };
      };
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
  user.extraGroups = [ "video" "dialout" ];

  home.configFile = {
    "gtk-3.0/bookmarks".text = ''
      file://${homeDir}/Downloads
      file://${homeDir}/Documents
    '';
  };
}

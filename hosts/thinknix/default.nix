{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  imports = [
    ./sound.nix
    ./throttled.nix
    ./hardware-configuration.nix
    ../personal.nix
    ./etc.nix
  ];

  user.packages = with pkgs; [
    appimage-run
    ffmpeg
    xcolor
    (writeShellScriptBin "," ''
      set -euo pipefail
      usage() {
        >&2 echo "Usage: , <package> <executable> [-- <args>...]"
      }
      if [ $# -lt 1 ]; then
        usage; exit 1
      fi
      if [[ "$1" =~ ^.*#(.*)$ ]]; then
        package="$1"
        if [ $# -lt 2 ] || [ "$2" = "--" ]; then
          executable="''${BASH_REMATCH[1]}"
        else
          executable="$2"
        fi
      else
        package="nixpkgs#$1"
        if [ $# -lt 2 ] || [ "$2" = "--" ]; then
          executable="$1"
        else
          executable="$2"
        fi
      fi
      shift
      [ $# -gt 0 ] && shift
      [ $# -gt 0 ] && [ "$1" = "--" ] && shift
      nix shell "$package" -c "$executable" "$@"
    '')
    # user.comma
    gparted
    gnome3.nautilus
    s-tui
    jq
    nethogs
    nixpkgs-fmt
    screenkey
    signal-desktop
    magic-wormhole
    termdown
    unrar
    unzip
    user.based-connect
    vlc
    xorg.xwininfo
    xournal
    xtitle
    xvkbd
  ] ++ flake-packages;

  hardware.usb.wakeupDisabled = [
    {
      # Logitech, Inc. G3 (MX518) Optical Mouse
      vendor = "046d";
      product = "c051";
    }
  ];

  # Timer
  services.atd.enable = true;

  modules = {
    bindings.enable = true;
    shell = {
      bpytop.enable = true;
      color-optimization.enable = true;
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      pass.enable = true;
      weechat.enable = true;
      wget.enable = true;
      youtube-dl.enable = true;
      zsh.enable = true;
    };
    scripts = {
      screen-record.enable = true;
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
      # bspwm.enable = true;
      xmonad.enable = true;
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
        enableMail = true;
      };
      vim.enable = true;
    };
    services = {
      screendrawing.enable = true;
      syncthing.enable = true;
      polybar.enable = false;
      dunst.enable = true;
      greenclip.enable = true;
      hotplug.enable = true;
      flameshot.enable = true;
      network-monitoring.enable = true;
      picom.enable = true;
    };
    work.meisterlabs.enable = true;
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
}

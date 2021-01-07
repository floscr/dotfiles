{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
    ../personal.nix
  ];

  user.packages = with pkgs; [
    gparted
    jq
    unrar
    unzip
  ];

  modules = {
    bindings.enable = true;
    shell = {
      direnv.enable     = true;
      git.enable        = true;
      gnupg.enable      = true;
      pass.enable       = true;
      zsh.enable        = true;
      youtube-dl.enable = true;
    };
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      keyboard.enable = true;
      external-display.enable = true;
      scanner.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
      };
      sensors.enable = true;
    };
    dev = {
      node.enable = true;
      nim.enable = true;
      docker.enable = true;
      mysql.enable = true;
    };
    desktop = {
      bspwm.enable = true;
      apps = {
        rofi.enable = true;
        font-manager.enable = true;
        transmission.enable = true;
      };
      graphics = {
        # gimp.enable = true;
        # inkscape.enable = true;
      };
      media = {
        spotify.enable = true;
        mpv.enable = true;
      };
      browsers = {
        default = "firefox";
        chromium.enable = true;
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
      polybar.enable = true;
      dunst.enable = true;
      greenclip.enable = true;
      hotplug.enable = true;
    };
    work.meisterlabs.enable = true;
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

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fontconfig = {
      dpi = 180;
      defaultFonts.monospace = [ "Iosevka" ];
      useEmbeddedBitmaps = true;
    };
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome_5
      iosevka
      noto-fonts
      noto-fonts-cjk
      siji
      symbola
    ];
  };

  # Monitor backlight control
  programs.light.enable = true;
}

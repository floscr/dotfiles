{ pkgs, inputs, ... }: {
  # NOTE: Lenovo's T490 appears to be the closest to my E490.
  # REVIEW: Check https://github.com/NixOS/nixos-hardware for updates.
  imports = [ inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490 ];

  modules = {
    theme.mode = "dark";
    scripts = {
      screen_record.enable = true;
      measure.enable = true;
    };

    desktop = {
      enable = true;
      wm = "xmonad";
      dpi = 119;
      xmonad.enable = true;
      mime.enable = true;
      apps = {
        rofi.enable = true;
        transmission.enable = true;
        # font-manager.enable = true;
      };
      gaming = {
        steam.enable = true;
        emulators.n64.enable = true;
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
      graphics = {
        gimp.enable = true;
        # inkscape.enable = true;
      };
      media = {
        spotify.enable = true;
        mpv.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    cached-nix-shell
  ];

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      timeout = 1;
      systemd-boot = {
        configurationLimit = 10;
        enable = true;
        editor = false; # Fix a security hole in place for the sake of backwards compatibility.
      };
    };

    # Pretty boot loading screens.
    plymouth.enable = true;

    # Betsy's LUKS crypted root.
    initrd.luks.devices = {
      root = {
        device = "/dev/nvme0n1p2";
        preLVM = true;
        allowDiscards = true;
      };
    };

    # kernelParams = [ "psmouse.synaptics_intertouch=1" ];
    kernelModules = [ "kvm-intel" ];

    # Kernel tuning.
    kernel.sysctl = {
      # NOTE: An inotify watch consumes 1kB on 64-bit machines.
      "fs.inotify.max_user_watches" = 1048576; # default:  8192
      "fs.inotify.max_user_instances" = 1024; # default:   128
      "fs.inotify.max_queued_events" = 32768; # default: 16384
      "sysrq" = 1; # alt+prtsc
    };
  };

}

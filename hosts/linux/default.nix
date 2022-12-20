# Defaults across all my NixOS hosts.
{ inputs, config, lib, pkgs, options, ... }:

with lib;
with lib.my; {
  system = {
    stateVersion = config.home-manager.users.${config.user.name}.home.stateVersion;
    # Let `nixos-version --json` know about the Git revision of this flake.
    configurationRevision = with inputs; mkIf (self ? rev) self.rev;
  };

  # -- Networking
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false
  # here. Per-interface useDHCP will be mandatory in the future, so this
  # generated config replicates the default behaviour.
  networking.useDHCP = false;

  nix = {
    # Automatically detects files in the store that have identical contents.
    settings.auto-optimise-store = true;

    gc = {
      # Automatically run the Nix garbage collector daily.
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 10d";
    };
  };

  # Needs must.
  nixpkgs.config.allowUnfree = true;

  # Boot and console.
  boot = {
    # Use the latest Linux kernel.
    kernelPackages = mkDefault pkgs.linuxPackages_5_10;

    # Cattle not pets.
    tmpOnTmpfs = true;

    # Kernel.
    kernelModules = [ "tcp_bbr" ];
    kernel.sysctl = {
      # Bufferbloat mitigations + slight improvements in throughput and latency.
      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.core.default_qdisc" = "cake";
      # Fast Open is a TCP extension that reduces network latency by packing
      # data in the sender’s initial TCP SYN.
      # NOTE: Setting 3 = enable for both incoming and outgoing connections.
      "net.ipv4.tcp_fastopen" = 3;
    };
  };

  console.keyMap = "us";

  # Fix early console display.
  hardware.video.hidpi.enable = config.modules.desktop.hidpi;

  # Default low-level Linux system packages.
  environment = {
    systemPackages = with pkgs; [
      exfat
      hfsprogs
      ntfs3g
      openssl
      patchelf
      sshfs
      usbutils
      zlib
      zstd
    ];
  };

  # Location, timezone and internationalisation.
  location = {
    latitude = 12.5;
    longitude = 55.88;
  };
  time.timeZone = mkDefault "Europe/Vienna";
  i18n.defaultLocale = "en_US.UTF-8";

  # Security.
  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };

    # Prevent replacing the running kernel without reboot.
    protectKernelImage = true;
  };

  # Linux user and homedir settings.
  user = {
    extraGroups = [ "input" "disk" "audio" "video" "systemd-journal" ];
    initialHashedPassword = config.secrets.password;
  };

  users = {
    # Empty root password to begin with.
    extraUsers.root.initialHashedPassword = config.secrets.password;

    # Ensure only way to change users/groups is through this file.
    mutableUsers = false;
  };
}

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
    ../personal.nix
  ];

  user.packages = with pkgs; [
    nixpkgs-fmt
    brightnessctl
  ];

  modules = {
    bindings.enable = true;
    shell = {
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      pass.enable = true;
      youtube-dl.enable = true;
      zsh.enable = true;
    };
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      keyboard.enable = true;
    };
    dev = {
      nim.enable = true;
    };
    desktop = {
      xmonad.enable = true;
      apps = {
        rofi.enable = true;
      };
      media = {
        spotify.enable = true;
        mpv.enable = true;
      };
      browsers = {
        default = "brave";
        psd = true;
        brave.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
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
    theme.active = "opera";
  };
  hardware = {
    brillo.enable = true;
    opengl.driSupport32Bit = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  };
  # Fix the horrible color profile on the display from linux
  # Export your default color profile from the mac osx partition
  # Source: https://github.com/willtim/nixos/blob/52e730ec0d8288a3862538205cd8ff0fa2d1c159/desktop.nix#L151
  # xiccd apparently is buggy and cpu intensive
  # services.xserver.displayManager.sessionCommands = ''{pkgs.argyllcms}/bin/dispwin -I "~/.local/macbook-air-lcd.icc"'';

  programs.light.enable = true;
  user.extraGroups = [ "video" ];

  # services.xserver.libinput.enable = true;
  # services.xserver.libinput.disableWhileTyping = true;
}

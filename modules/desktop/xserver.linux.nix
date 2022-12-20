{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
{
  config = mkIf config.services.xserver.enable {
    environment.systemPackages = with pkgs; [
      xclip
      xdotool
      feh
    ];

    services.xserver.displayManager.sessionCommands = ''
      # Fix default cursor
      xsetroot -cursor_name left_ptr
    '';

    home.configFile."xtheme/80-dpi".text = ''
      #if X_RESOLUTION > 3840
        Xft.dpi: 144
      #endif
      #if X_RESOLUTION <= 3840
        Xft.dpi: 120
      #endif

      Xft.antialias: 1
      Xft.autohint: 0
      Xft.hinting: 1
      Xft.hintstyle: hintslight
      Xft.lcdfilter: lcddefault
      Xft.rgba: rgb
    '';

    # Clean up leftovers, as much as we can
    system.userActivationScripts.cleanupHome = ''
      pushd $HOME
      rm -rf .compose-cache .nv .pki .dbus .fehbg
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';
  };
}

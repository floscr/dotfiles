# from https://github.com/macunha1/configuration.nix/blob/master/packages/uhk-agent.nix

# packages/uhk-agent -- UHK configuration agent overlay
#
# Ref: https://discourse.nixos.org/t/ultimate-hacking-keyboard-firmware-update/3001/6

{ config, lib, pkgs, stdenv, makeDesktopItem, ... }:

let
  pname = "uhk-agent";
  name = "${pname}-${version}";

  # version >1.3.0 causes uhk-agent to hang on launch
  # "Loading keyboard configuration. Hang tight!") -- literally tight.
  version = "1.5.15"; # Change at your own risk.

  src = builtins.fetchurl {
    url =
      "https://github.com/UltimateHackingKeyboard/agent/releases/download/v${version}/UHK.Agent-${version}-linux-x86_64.AppImage";
    sha256 = {
      "1.5.15" = "148aip9ryza18l285v7d294z6zwqd3qnz0hnqiw3cyblzivp0qmp";
    }."${version}";
  };

  desktopItem = makeDesktopItem {
    name = pname;
    desktopName = "UHK Agent";
    genericName = "Keyboard configuration";
    comment =
      "Agent is the configuration application of the Ultimate Hacking Keyboard";
    icon = "uhk-agent";
    terminal = "false";
    exec = pname;
    categories = "Utility;";
  };

  xdgDirs = builtins.concatStringsSep ":" [
    "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}"
    "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
    "$XDG_DATA_DIRS"
  ];

  appimageContents = pkgs.appimageTools.extractType2 { inherit name src; };

in
pkgs.appimageTools.wrapType2 rec {
  inherit name src;

  # Uncomment in case debugging is necessary {{
  # runScript = pkgs.writeScript "run" ''
  #   #!${pkgs.stdenv.shell}

  #   export APPDIR=${pkgs.appimageTools.extractType2 { inherit name src; }}
  #   export APPIMAGE_SILENT_INSTALL=1

  #   # NOTE: Inspect the script running environment here
  #   echo "INSPECT: ''${GIO_EXTRA_MODULES:-no extra modules!}"
  #   echo "INSPECT: ''${GSETTINGS_SCHEMA_DIR:-no schemas!}"
  #   echo "INSPECT: ''${XDG_DATA_DIRS:-no data dirs!}"

  #   cd $APPDIR
  #   exec ./AppRun "$@"
  # '';
  # }} Uncomment in case debugging is necessary

  multiPkgs = null;

  # Borrows Electron packages from Atom
  # Ref: https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/atom/env.nix
  extraPkgs = pkgs:
    with pkgs;
    atomEnv.packages ++ [
      pciutils
      libusb1
      xorg.libxshmfence
      libdrm
      libxkbcommon
      mesa

      # Additional electron dependencies (pinning version)
      at-spi2-atk
      at-spi2-core
    ];

  extraInstallCommands = ''
    ln -s "$out/bin/${name}" "$out/bin/uhk-agent"
    mkdir -p $out/etc/udev/rules.d

    cat > $out/etc/udev/rules.d/50-uhk60.rules <<EOF
    # Ultimate Hacking Keyboard rules
    # These are the udev rules for accessing the USB interfaces of the UHK as non-root users.
    # Copy this file to /etc/udev/rules.d and physically reconnect the UHK afterwards.
    SUBSYSTEM=="input", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="612[0-7]", GROUP="input", MODE="0660"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="612[0-7]", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="612[0-7]", TAG+="uaccess"
    EOF

    mkdir -p $out/share/applications
    cp ${desktopItem}/share/applications/* $_

    mkdir -p $out/share/icons/hicolor
    # Iterate over icons and copy them to out, dynamically
    for icon_file in $(find ${appimageContents}/usr/share/icons/hicolor -name uhk-agent.png); do
      # sed erases the appimageContents path
      install -m 444 -D $icon_file $out/share/icons/$(echo $icon_file | sed 's/.*hicolor/hicolor/')
    done
  '';

  profile = ''
    export XDG_DATA_DIRS="${xdgDirs}"
    export APPIMAGE=''${APPIMAGE-""} # Kill a seemingly useless error message
  '';

  meta = with lib; {
    description = ''
      Agent is the configuration application of the Ultimate Hacking Keyboard
    '';

    longDescription = ''
      The Ultimate Hacking Keyboard is a split mechanical keyboard which utilizes
      Cherry MX-style switches. It's also a fully programmable keyboard which
      can be vastly customized through this agent for your needs.
    ''; # adapted from https://ultimatehackingkeyboard.com/

    homepage = "https://ultimatehackingkeyboard.com/start/agent";
    license = licenses.unfreeRedistributable;
    maintainers = with maintainers; [ macunha1 ];
    platforms = [ "i386-linux" "x86_64-linux" ];
  };
}

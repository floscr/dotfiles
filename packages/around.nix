{ stdenv
, lib
, buildFHSUserEnv
, appimageTools
, pkgs
, fetchurl
, autoPatchelfHook
, gsettings-desktop-schemas
, gtk3
}:

appimageTools.wrapType2 {
  name = "around";
  src = fetchurl {
    url = https://downloads.around.co/Around.AppImage;
    sha256 = "sha256-x1I0+ehCbyr24sf/MWaaCvByWtPUAmKdYQzLhF3GNRs=";
  };

  profile = ''
    export XDG_DATA_DIRS=${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk3}/share/gsettings-schemas/${gtk3.name}:$XDG_DATA_DIRS
  '';

  extraPkgs = pkgs: (with pkgs;
    [
      alsaLib
      at-spi2-atk
      at-spi2-core
      atk
      cairo
      cmake
      cups
      dbus
      expat
      gcc
      gdk-pixbuf
      glib
      libdrm
      mesa
      nspr
      nss
      pango
      libpulseaudio
    ]) ++ (with pkgs.xorg;
    [
      libX11
      libXcomposite
      libXdamage
      libXext
      libXfixes
      libXrandr
      libXtst
      libxcb
      libxshmfence
      xcbutilimage
      xcbutilkeysyms
    ]);

  meta = with stdenv.lib; {
    homepage = "around.co";
    description = "gmeet like video calls";
  };
}

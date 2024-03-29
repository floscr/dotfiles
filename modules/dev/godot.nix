{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.godot;

  arch = "64";
  version = "3.3.2";
  pkg = pkgs.stdenv.mkDerivation {
    name = "godot-mono-unwrapped";
    buildInputs = [ pkgs.unzip ];
    unpackPhase = "unzip $src";
    version = version;
    src = pkgs.fetchurl {
      url = "https://downloads.tuxfamily.org/godotengine/${version}/mono/Godot_v${version}-stable_mono_x11_${arch}.zip";
      sha256 = "036i3hqlabk736pg9l7qraj5mq2jdg9gfka7g22fyrr3hvz8jv4a";
    };
    installPhase = ''
      cp -r . $out
    '';
  };
  godot = pkgs.buildFHSUserEnv {
    name = "godot-mono";
    targetPkgs = pkgs: (with pkgs;
      [
        alsaLib
        dotnetCorePackages.sdk_7_0
        libGL
        libpulseaudio
        udev
        unstable.xorg.libX11
        xorg.libXcursor
        xorg.libXext
        xorg.libXi
        xorg.libXinerama
        xorg.libXrandr
        xorg.libXrender
        zlib
      ]);
    runScript = "${pkg.outPath}/Godot_v${version}-stable_mono_x11_${arch}/Godot_v${version}-stable_mono_x11.${arch}";
  };
in
{
  options.modules.dev.godot = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      godot
      pkgs.aseprite
    ];
  };
}

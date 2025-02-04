{ lib, stdenv, replace, xorg, fetchurl, pkgs, makeWrapper, fetchFromGitHub }:

# Doesnt really work how i need it
stdenv.mkDerivation rec {
  name = "space2ctrl";
  src = fetchFromGitHub {
    owner = "floscr";
    repo = "Space2Ctrl";
    rev = "98da565";
    sha256 = "sha256-l4AT13FlKpdYU0gKGZrgI3BubWIt/GOb9J8SCcfaNOY=";
  };
  buildInputs = [
    replace

    (xorg.inputproto or xorg.xorgproto)
    (xorg.recordproto or xorg.xorgproto)
    xorg.libX11
    xorg.libXext
    xorg.libXi
    xorg.libXtst
    xorg.xinput
  ];

  # Force 'make install' to use $out
  makeFlags = [ "PREFIX=$(out)" ];

  # Avoid triggering a keypress event on startup by commenting out that code
  pre1 = "// TODO: document why the following event is needed";
  post1 = "/*";
  pre2 = "if (!XRecordEnableContext";
  post2 = "*/ if (!XRecordEnableContext";

  # preBuild = ''
  #   ${replace}/bin/replace "$pre1" "$post1" -- Space2Ctrl.cpp
  #   ${replace}/bin/replace "$pre2" "$post2" -- Space2Ctrl.cpp
  # '';
}

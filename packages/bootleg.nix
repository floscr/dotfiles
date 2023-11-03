# for nix-shell
# with import <nixpkgs> {};

{ lib, stdenv, fetchurl, pkgs, makeWrapper }:

stdenv.mkDerivation rec {
  pname = "bootleg-prebuilt";
  version = "0.1.9";

  src = fetchurl (if stdenv.isLinux then {
    url = "https://github.com/retrogradeorbit/bootleg/releases/download/v${version}/bootleg-${version}-linux-amd64.tgz";
    sha256 = "05ris3digl8hc7r8c6wlbqkxqis22ddr55kr6891a6z2hgr2mm2l";
  } else {
    # fail
  });

  dontUnpack = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    tar xf ${src}
    mkdir -p $out/bin/
    cp ./bootleg $out/bin/
    cp -av $out/bin/bootleg $out/bin/bootleg-wrapped
    makeWrapper ${pkgs.steam-run}/bin/steam-run $out/bin/bootleg-wrapped --add-flags $out/bin/bootleg
  '';

  postInstallCheck = ''
    $bin/bootleg --version >/dev/null
  '';

  meta = with lib; {
    description = ''bootleg by retrogradeorbit'';
    platforms = with platforms; linux;
    downloadPage = "https://github.com/retrogradeorbit/bootleg";
    inherit version;
  };
}

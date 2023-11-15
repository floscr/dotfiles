{ stdenv, lib, fetchurl, ... }:

# TODO remove this after https://github.com/NixOS/nixpkgs/issues/260161
stdenv.mkDerivation rec {
  name = "babashka";
  version = "1.3.186";

  src = fetchurl {
    url = "https://github.com/babashka/babashka/releases/download/v${version}/babashka-${version}-linux-amd64-static.tar.gz";
    sha256 = "sha256-+pLWrVo5cRpoY5Bb2gGjf/DvdbZG5JNxKMmsw0hfG58=";
  };

  unpackPhase = ''
    tar xvf $src
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv bb $out/bin/bb
  '';

  meta = with lib; {
    homepage = "https://github.com/babashka/babashka";
    description = "A Clojure babushka for the grey areas of Bash";
    platforms = platforms.linux;
  };
}

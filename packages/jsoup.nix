{ stdenv, lib, fetchFromGitHub, pkgs }:

# Doesn't work would need something like clj-nix to fetch leinigen packages
stdenv.mkDerivation rec {
  pname = "jsoup";
  version = "0.0.1";
  src = fetchFromGitHub {
    owner = "jaydeesimon";
    repo = "pod-jaydeesimon-jsoup";
    rev = "3f387cdf366cd9cc65e46f2cf3fb6d3017e51dff";
    sha256 = "sha256-WcQWKYeUtueyqX958ygDYU7qG4LCu3zIqDJlH6LZdUQ=";
  };
  nativeBuildInputs = [
    pkgs.openjdk17
    pkgs.leiningen
    pkgs.clojure
  ];
  buildInputs = [
    pkgs.graalvm-ce
  ];

  prePatch = ''
    patchShebangs scripts/compile
  '';


  buildPhase = ''
    export PATH="${pkgs.graalvm-ce}/bin:$PATH"
    export JAVA_HOME="${pkgs.graalvm-ce}"
    export GRAALVM_HOME="${pkgs.graalvm-ce}"
    export HOME=$(pwd)
    patchShebangs src/compile
    cat script/compile
    . script/compile
  '';

  installPhase = ''
    mkdir -p $out/bin/
    cp ./pod-jaydeesimon-jsoup $out/bin/pod-jaydeesimon-jsoup
  '';
}

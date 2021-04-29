{ lib, pkgs }:

with pkgs;

pkgs.stdenv.mkDerivation rec {
  pname = "based-connect";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "Denton-L";
    repo = "based-connect";
    rev = "335c66527793198f566946d4d57a9d830b441d25";
    sha256 = "sha256-OAoQS/4dj7Q5Jn7g5cYmT+/e7QYTGWl5TGuSK2sWeFs=";
  };

  buildInputs = [
    bluez
  ];

  makeFlags = [ "DESTDIR=$(out)" ];

  buildPhase = ''
    make -j
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp -r ./* $out/bin
  '';
}

{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "alpacacpp";
  version = "0.0.1";
  src = fetchFromGitHub {
    owner = "zanussbaum";
    repo = "gpt4all.cpp";
    rev = "f8fdcccc5d253229808c0ceb9c5faae1ba42f68c";
    sha256 = "sha256-80ff7577GhHYgxy5PcD5lTbVWZG5L3NyFRZXOq3bieg=";
  };
  nativeBuildInputs = [ ];
  buildInputs = [ ];

  buildPhase = ''
    make chat
  '';

  installPhase = ''
    mkdir -p $out/bin/
    cp ./chat $out/bin/chat
  '';
  meta = with lib; {
    description = "Run a fast ChatGPT-like model locally on your device";
    homepage = "https://github.com/antimatter15/alpaca.cpp";
    license = licenses.gpl3;
    platforms = platforms.linux;
  };
}

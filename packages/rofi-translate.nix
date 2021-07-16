{ lib, stdenv, makeWrapper, fetchFromGitHub, coreutils, rofi, translate-shell }:

stdenv.mkDerivation rec {
  pname = "rofi-translate";
  version = "84356fd2e097166be022326e92feaf0b2eb15b6d";

  src = fetchFromGitHub {
    owner = "garyparrot";
    repo = pname;
    rev = version;
    hash = "sha256:1dqj49l17b42rwpgrx3gym9g6ds1cpapqdyxrih43allibg4kw0q";
  };

  dontBuild = true;

  buildInputs = [ coreutils rofi translate-shell ];

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    install -vD -t $out/bin \
      rofi_trans rofi_trans_brief rofi_trans_delete rofi_trans_verbose rofi_verbose
    sed -i 's/export transHistory=.*/export transHistory="\$HOME\/.cache\/rofi-translate\/history"/' $out/bin/rofi_trans
    sed -i 's/export transAudioCacheDir=.*/export transAudioCacheDir="\$HOME\/.cache\/rofi-translate\/audio"/' $out/bin/rofi_trans
    sed -i 's/export saveAudio=.*/export saveAudio="disabled"/' $out/bin/rofi_trans
    sed -i 's/export transTarget=.*/export transTarget="en_US"/' $out/bin/rofi_trans
    wrapProgram $out/bin/rofi_trans \
      --prefix PATH : $out/bin \
      --prefix PATH : ${lib.makeBinPath [ coreutils rofi translate-shell ]}
  '';

  meta = with lib; {
    description = "Rofi translate script based on translate-shell";
    homepage = "https://github.com/garyparrot/rofi-translate";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.all;
  };
}

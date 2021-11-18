{ lib, fetchzip }:

# This package differs from the stock package in that
# this package installs the separate, non-variable ttf files instead of the single variable FiraCode-VF.ttf  file,
# because vscode cannot seem to read the FiraCode-Retina weight style from the FiraCode-VF.ttf file.
# From https://github.com/tonsky/FiraCode/wiki/VS-Code-Instructions:
# > To use Retina weight, change Font name to FiraCode-Retina if on macOS (exactly that, no spaces).
# > It might be necessary to install the separate .ttf files and not the single variable FiraCode-VF.ttf file:

let
  version = "5.2";
in
fetchzip {
  name = "fira-code-static";

  url = "https://github.com/tonsky/FiraCode/releases/download/${version}/Fira_Code_v${version}.zip";

  # only extract the variable font because everything else is a duplicate
  postFetch = ''
    mkdir -p $out/share/fonts
    unzip -j $downloadedFile '*.ttf' -x '*-VF.ttf' -d $out/share/fonts/truetype
  '';

  sha256 = "1mghf53yf6i7xn0gr2ixrd6mr7wcjy1y538i1k92c82qs7y2lb8j";

  meta = {
    homepage = "https://github.com/tonsky/FiraCode";
    description = "Monospace font with programming ligatures";
    longDescription = ''
      Fira Code is a monospace font extending the Fira Mono font with
      a set of ligatures for common programming multi-character
      combinations.

      This package differs from the stock package in that this package installs non-variable font files because vscode cannot seem to read the FiraCode-Retina font style from the *-VF.ttf file.
    '';
    license = lib.licenses.ofl;
    # maintainers = [ lib.maintainers.rycee ];
    platforms = lib.platforms.all;
  };
}

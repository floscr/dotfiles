{ lib, pkgs }:

with pkgs;

rustPlatform.buildRustPackage rec {
  pname = "frece";
  version = "1.0.4";

  src = fetchFromGitHub {
    owner = "YodaEmbedding";
    repo = pname;
    rev = "8f40889dbe87dd5cc077142a924527fccc8b780b";
    sha256 = "sha256-KuFj6qTwgplIuXb/d05ibp2uovxhhi0L61eYgx+rSPo=";
  };

  buildInputs = [];

  checkPhase = null;
  cargoSha256 = "sha256-FEPRDZjbmiU6+M7J1YnWT5n0pQ7VLlPPIc87BvEtvi8=";

  meta = with lib; {
    description =
      "A standalone widget system made in Rust to add AwesomeWM like widgets to any WM";
    homepage = "https://github.com/elkowar/eww";
    license = licenses.mit;
    maintainers = with maintainers; [ "foo" ];
  };
}

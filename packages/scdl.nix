{ lib, python3Packages, fetchFromGitHub }:

python3Packages.buildPythonApplication rec {
  pname = "scdl";
  version = "2.7.5";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "flyingrub";
    repo = pname;
    rev = "e8a0f921668e5cdc61e00ce9f42d0f8546b8167d";
    sha256 = "sha256-vP/07vXeMnKOQ9OIZj8+9kZ3voHvspQqQNpWyPPkvUg=";
  };


  # src = fetchPypi {
  #   inherit pname version;
  #   sha256 = "60284b7b058040d4847f2e4b0ab906b10e959d51f976a0188641e8e10685474f";
  # };

  propagatedBuildInputs = with python3Packages; [
    docopt
    mutagen
    termcolor
    requests
    clint
    pathvalidate
    soundcloud-v2
  ];

  # No tests in repository
  doCheck = false;

  pythonImportsCheck = [ "scdl" ];

  meta = with lib; {
    description = "Download Music from Souncloud";
    homepage = "https://github.com/flyingrub/scdl";
    license = licenses.gpl2Only;
    maintainers = with maintainers; [ marsam ];
    mainProgram = "scdl";
  };
}

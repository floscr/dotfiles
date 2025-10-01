{ buildGoModule
, fetchFromGitHub
, lib
}:

buildGoModule rec {
  pname = "uniclip";
  version = "2.3.6";

  src = fetchFromGitHub {
    owner = "quackduck";
    repo = "uniclip";
    rev = "v${version}";
    hash = "sha256-j870woiQkX/vmeVQyTnN/qd/wavZPtP590fndkm0klM=";
  };

  vendorHash = "sha256-ugrWrB0YVs/oWAR3TC3bEpt1VXQC1c3oLrvFJxlR8pw=";

  meta = {
    description = "Cross-platform shared clipboard";
    homepage = "https://github.com/quackduck/uniclip";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ rksm ];
  };
}

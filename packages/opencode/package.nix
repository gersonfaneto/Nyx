{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation rec {
  pname = "opencode";
  version = "1.0.35";
  src = fetchzip {
    url = "https://github.com/sst/opencode/releases/download/v${version}/opencode-linux-x64.zip";
    sha256 = "sha256-3WbDdXVUYNatsTcLFdbBgtYx3E4zScYznj3A9XvK4Es=";
  };
  dontUnpack = false;
  installPhase = ''
    install -Dm755 opencode $out/bin/opencode
  '';
}

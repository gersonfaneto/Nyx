{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation rec {
  pname = "opencode";
  version = "0.15.14";
  src = fetchzip {
    url = "https://github.com/sst/opencode/releases/download/v${version}/opencode-linux-x64.zip";
    sha256 = "sha256-0ZMpdScnMzy6obd7jpD1WP83jrfJRC1cUob4mLbdxpE=";
  };
  dontUnpack = false;
  installPhase = ''
    install -Dm755 opencode $out/bin/opencode
  '';
}

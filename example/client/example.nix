{ pkgs, stdenv, ... }:
let
  version = "0.0.1";

in stdenv.mkDerivation {
  name = "servant-auth-hmac-example-client-${version}";

  buildInputs = with pkgs.nodePackages; [
    typescript
    rollup
  ];

  src = ./.;

  buildPhase = ''
    ./build.sh
  '';

  installPhase = ''
    mkdir -p $out
    mv "./result/static" $out
  '';
}

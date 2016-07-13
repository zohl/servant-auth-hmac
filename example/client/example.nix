{ pkgs, stdenv, ... }:
let
  version = "0.0.1";
  buildDir = "./static";

in stdenv.mkDerivation {
  name = "servant-auth-hmac-example-client-${version}";

  buildInputs = with pkgs.nodePackages; [
    typescript
    rollup
  ];

  src = ./src;


  buildPhase = ''
    rm -rf ${buildDir}
    mkdir -p ${buildDir}
    mkdir -p ${buildDir}/tmp

    tsc --outDir ${buildDir}/tmp \
        --noEmitOnError          \
        --target ES6             \
        "./"*.ts

    rollup -o ${buildDir}/app.js \
      ${buildDir}/tmp/app.js

    rm -r ${buildDir}/tmp
  '';

  installPhase = ''
    mkdir -p $out
    mv "${buildDir}" $out
  '';
}

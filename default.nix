{ mkDerivation, aeson, base, base64-bytestring, blaze-html
, blaze-markup, bytestring, cereal, cryptonite, deepseq, http-media
, HUnit, memory, mtl, servant, servant-server, stdenv, text, time
, unix, wai, warp
}:
mkDerivation {
  pname = "servant-auth-hmac";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring bytestring cereal memory servant
    servant-server wai
  ];
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring cereal http-media mtl
    servant servant-server text unix wai warp
  ];
  testHaskellDepends = [
    base bytestring cereal cryptonite deepseq HUnit servant-server time
  ];
  license = stdenv.lib.licenses.gpl3;
}

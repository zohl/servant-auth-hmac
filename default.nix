{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-html, blaze-markup, bytestring, cereal, containers
, cryptonite, data-default, deepseq, exceptions, http-media
, http-types, HUnit, memory, mtl, random, servant, servant-server
, stdenv, text, time, transformers, unix, wai, warp
}:
mkDerivation {
  pname = "servant-auth-hmac";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring cereal data-default
    exceptions http-types memory servant servant-server time wai
  ];
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring cereal containers
    http-media mtl random servant servant-server text transformers unix
    wai warp
  ];
  testHaskellDepends = [
    base bytestring cereal cryptonite deepseq HUnit servant-server time
  ];
  license = stdenv.lib.licenses.gpl3;
}

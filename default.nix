{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-html, blaze-markup, bytestring, case-insensitive, cereal
, containers, cryptonite, data-default, deepseq, exceptions
, hspec-wai, hspec-wai-json, hspec2, http-media, http-types, memory
, mtl, QuickCheck, random, servant, servant-blaze, servant-server
, stdenv, string-class, text, time, transformers, unix, wai, warp
}:
mkDerivation {
  pname = "servant-auth-hmac";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    cereal cryptonite data-default exceptions http-types memory servant
    servant-server string-class time wai
  ];
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring cereal containers
    data-default http-media mtl random servant servant-blaze
    servant-server string-class text transformers unix wai warp
  ];
  testHaskellDepends = [
    base bytestring cereal cryptonite deepseq hspec-wai hspec-wai-json
    hspec2 QuickCheck servant-server time
  ];
  license = stdenv.lib.licenses.gpl3;
}

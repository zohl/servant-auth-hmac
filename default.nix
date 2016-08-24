{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-html, blaze-markup, bytestring, case-insensitive, cereal
, containers, cryptonite, data-default, exceptions, hspec
, hspec-expectations, hspec-wai, http-media, http-types, memory
, mtl, random, servant, servant-blaze, servant-server, stdenv
, string-class, text, time, transformers, unix, wai, wai-extra
, warp, with-location
}:
mkDerivation {
  pname = "servant-auth-hmac";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    cereal cryptonite data-default exceptions http-types memory servant
    servant-server string-class time transformers wai
  ];
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring cereal containers
    data-default http-media mtl random servant servant-blaze
    servant-server string-class text transformers unix wai warp
  ];
  testHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive cereal
    containers cryptonite data-default hspec hspec-expectations
    hspec-wai http-types random servant servant-server string-class
    time transformers wai wai-extra with-location
  ];
  license = stdenv.lib.licenses.bsd3;
}

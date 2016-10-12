{ stdenv, mkDerivation, base, base-compat, bytestring, case-insensitive
 , hspec, hspec-core, hspec-expectations, http-types, QuickCheck
 , text, transformers, wai, wai-extra, with-location
 }:
 mkDerivation {
   pname = "hspec-wai";
   version = "0.8.0";
   sha256 = "0h8i78kjc5bv8aly4r7m5p2a8mw5j9ms8qm79mkwqadx877y4zlb";
   libraryHaskellDepends = [
     base base-compat bytestring case-insensitive hspec-core
     hspec-expectations http-types QuickCheck text transformers wai
     wai-extra with-location
   ];
   testHaskellDepends = [
     base base-compat bytestring case-insensitive hspec hspec-core
     hspec-expectations http-types QuickCheck text transformers wai
     wai-extra with-location
   ];
   homepage = "https://github.com/hspec/hspec-wai#readme";
   description = "Experimental Hspec support for testing WAI applications";
   license = stdenv.lib.licenses.mit;
 }

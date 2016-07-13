{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}

import Test.HUnit
import Control.Monad
import Data.Time.Clock           (getCurrentTime, addUTCTime)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.IO
import System.Exit
import Control.Concurrent (threadDelay)
import Servant.Server.Experimental.Auth.HMAC.Internal
import Servant (Proxy(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Crypto.Random (drgNew)
import Crypto.Hash (HashAlgorithm, SHA512, SHA384, SHA256)
import Crypto.Cipher.Types (BlockCipher, ctrCombine, cbcEncrypt, cbcDecrypt, cfbEncrypt, cfbDecrypt)
import Crypto.Cipher.AES (AES256, AES192, AES128)


tests :: [Test]
tests = []

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]

  Counts {cases, tried, errors, failures} <- runTestTT $ TestList tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure



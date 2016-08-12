{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import AuthAPI
import Data.IORef (newIORef, readIORef)
import Data.Aeson (encode)
import Data.Default
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import Servant.Server.Experimental.Auth.HMAC
import qualified Data.Map as Map
import Servant (Proxy(..))
import Servant.Server (Context ((:.), EmptyContext), serveWithContext)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hWWWAuthenticate, hAuthorization)
import Network.Wai.Test (SResponse(simpleHeaders,simpleStatus))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.CaseInsensitive (CI(..))


main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app def) $ do

  describe "POST /login" $ do
    it "rejects a request with wrong username/password" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = "mr_foo"
            , laPassword = "password"
            }

      request methodPost "/login" [(hContentType, "application/json")] loginArgs
        `shouldRespondWith` 403


    it "accepts a request with correct username/password" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = "mr_foo"
            , laPassword = "password1"
            }
      request methodPost "/login" [(hContentType, "application/json")] loginArgs
        `shouldRespondWith` 200


  describe "GET /secret" $ do

    it "rejects a request without authoriaztion header" $
      get "/secret/mr_bar" `shouldRespondWith` 401 {
          matchHeaders = [hWWWAuthenticate <:> "HMAC"]
        , matchBody = Just . BSLC8.pack . show $ NotAuthoirized
        }

    it "rejects a request with incorrect authorization header" $ do
      let s = "nope"
      let r = request methodGet "/secret/mr_bar" [("Authorization", s)] ""
      r `shouldRespondWith` 403 {
          matchBody = Just . BSLC8.pack . show $ BadAuthorizationHeader s
        }

    it "rejects a request without appropriate parameters" $ do
      let s = "HMAC id=\"mr_bar\",timestamp=\"0\""
      let r = request methodGet "/secret/mr_bar" [("Authorization", s)] ""
      r `shouldRespondWith` 403 {
          matchBody = Just . BSLC8.pack . show $   AuthorizationParameterNotFound "hash"
        }

    it "rejects an expired request" $ do
      let s = "HMAC hash=\"\",id=\"mr_bar\",timestamp=\"0\""
      let r = request methodGet "/secret/mr_bar" [("Authorization", s)] ""
      r `shouldRespondWith` 403


    it "rejects a request without non-existing token" $ do
      let s = "HMAC hash=\"\",id=\"mr_bar\",timestamp=\"10000000000\""
      let r = request methodGet "/secret/mr_bar" [("Authorization", s)] ""
      r `shouldRespondWith` 403 {
          matchBody = Just . BSLC8.pack . show $ TokenNotFound "mr_bar"
        }

    it "rejects a request with incorrect signature" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = "mr_bar"
            , laPassword = "letmein"
            }
      let login = request methodPost "/login" [(hContentType, "application/json")] loginArgs

      let s = "HMAC hash=\"\",id=\"mr_bar\",timestamp=\"10000000000\""
      let r = request methodGet "/secret/mr_bar" [("Authorization", s)] ""

      (login >> r) `shouldRespondWith` 403


app :: AuthHmacSettings -> IO Application
app authSettings = do
  storage <- newIORef $ Map.empty

  let authSettings' = ($ authSettings) $ \(AuthHmacSettings {..}) -> AuthHmacSettings {
      ahsGetToken = \username -> (Map.lookup username) <$> (readIORef storage)
    , ..
    }

  return $ serveWithContext
    (Proxy :: Proxy AuthAPI)
    ((defaultAuthHandler authSettings') :. EmptyContext)
    (serveAuth storage authSettings')





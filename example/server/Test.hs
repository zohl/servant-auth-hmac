{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import AuthAPI
import Data.IORef (newIORef, readIORef)
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
import Network.Wai.Test (SResponse(simpleHeaders,simpleStatus))

import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI(..))



main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app def) $
  describe "protected uri" $ do
    it "responds with 401 on request without authoriaztion header" $
      get "/secret/mr_bar" `shouldRespondWith` 401

    it "responds with 403 on request with incorrect authorization header" $ do
      let r = request methodGet "/secret/mr_bar" [("Authorization", "nope")] ""
      r `shouldRespondWith` 403


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





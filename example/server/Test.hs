{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE CPP               #-}

import AuthAPI
import Control.Monad (guard)
import Control.Applicative ((<$))
import Data.IORef (newIORef, readIORef)
import Data.Aeson (encode, decode)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.String.Class (ConvStrictByteString(..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.Wai (Application)
import Test.Hspec (Spec, hspec, describe, it)
import Test.Hspec.Wai ((<:>), MatchBody(..))
import Test.Hspec.Wai (request, matchHeaders, matchBody, shouldRespondWith, liftIO, with, get)
import Servant.Server.Experimental.Auth.HMAC
import qualified Data.Map as Map
import Servant (Proxy(..))
import Servant.Server (Context ((:.), EmptyContext), serveWithContext)
import Network.HTTP.Types (Header, methodGet, methodPost)
import Network.HTTP.Types.Header (hWWWAuthenticate, hAuthorization, hContentType)
import Network.Wai.Test (SResponse(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app (def :: AuthHmacSettings)) $ do

  describe "POST /login" $ do
    let username = "mr_foo"

    it "rejects a request with wrong username/password" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = username
            , laPassword = "password"
            }
      let r = request methodPost "/login" [(hContentType, "application/json")] loginArgs
      r `shouldRespondWith` 403

    it "accepts a request with correct username/password" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = username
            , laPassword = "password1"
            }
      let r = request methodPost "/login" [(hContentType, "application/json")] loginArgs
      r `shouldRespondWith` 200


  describe "GET /secret" $ do
    let username = "mr_bar"

    it "rejects a request without authoriaztion header" $ do
      let r = get ("/secret/" <> username)
      r `shouldRespondWith` 401 {
          matchHeaders = [hWWWAuthenticate <:> "HMAC"]
        , matchBody = bodyEquals . BSLC8.pack . show $ NotAuthoirized
        }

    it "rejects a request with incorrect authorization header" $ do
      let s = "nope"
      let r = request methodGet ("/secret/" <> username) [("Authorization", s)] ""
      r `shouldRespondWith` 403 {
        matchBody = bodyEquals . BSLC8.pack . show $ BadAuthorizationHeader s
        }

    it "rejects a request without appropriate parameters" $ do
      let r = request methodGet ("/secret/" <> username) [mkAuthHeader "" "" Nothing] ""
      r `shouldRespondWith` 403 {
          matchBody = bodyEquals . BSLC8.pack . show $ AuthorizationParameterNotFound "timestamp"
        }

    it "rejects an expired request" $ do
      let hdr = mkAuthHeader "" "" $ Just (posixSecondsToUTCTime 0)
      let r = request methodGet ("/secret/" <> username) [hdr] ""
      r `shouldRespondWith` 403 {
          matchBody = bodyStartsWith "RequestExpired "
        }


    it "rejects a request without non-existing token" $ do
      hdr <- liftIO $ mkAuthHeader (BSC8.unpack username) "" . Just <$> getCurrentTime
      let r = request methodGet ("/secret/" <> username)  [hdr] ""
      r `shouldRespondWith` 403 {
          matchBody = bodyEquals . BSLC8.pack . show $ TokenNotFound username
        }

    it "rejects a request with wrong signature" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = BSC8.unpack username
            , laPassword = "letmein"
            }
      _ <- request methodPost "/login" [(hContentType, "application/json")] loginArgs

      hdr <- liftIO $ mkAuthHeader (BSC8.unpack username) "" . Just <$> getCurrentTime
      let r = request methodGet ("/secret/" <> username) [hdr] ""

      r `shouldRespondWith` 403 {
          matchBody = bodyStartsWith "IncorrectHash "
        }


    it "accepts a request with correct signature" $ do
      let loginArgs = encode $ LoginArgs {
              laUsername = BSC8.unpack username
            , laPassword = "letmein"
            }

      (SResponse {..}) <- request methodPost "/login" [(hContentType, "application/json")] loginArgs

      currentTime <- liftIO $ getCurrentTime

      let hash = Base64.encode $ getRequestHash
            (def::AuthHmacSettings)
            (maybe "" id (decode simpleBody))
            (BSC8.unpack username)
            currentTime
            ("/secret/" <> username)
            "GET"
            []
            ""

      let hdr = mkAuthHeader (BSC8.unpack username) hash (Just currentTime)
      let r = request methodGet ("/secret/" <> username) [hdr] ""

      r `shouldRespondWith` 200 {
          matchBody = bodyStartsWith "\"Freedom is Slavery\""
        }


mkAuthHeader :: AuthHmacAccount -> BS.ByteString -> Maybe UTCTime -> Header
mkAuthHeader account hash mt = let
  timestampStrings = maybe [] (\timestamp -> [
      ",timestamp=\""
    , BSC8.pack . show $ ((truncate . utcTimeToPOSIXSeconds $ timestamp)::Integer)
    , "\""
    ]) $ mt
  in (hAuthorization, BS.concat $ [
      "HMAC "
    , "hash=\"", hash, "\""
    , ",id=\"", toStrictByteString account, "\""
    ] ++ timestampStrings)


app :: AuthHmacSettings -> IO Application
app authSettings = do
  storage <- newIORef $ Map.empty
  let tokenProvider username = (Map.lookup username) <$> (readIORef storage)

  return $ serveWithContext
    (Proxy :: Proxy AuthAPI)
    ((defaultAuthHandler tokenProvider authSettings) :. EmptyContext)
    (serveAuth storage)


bodyEquals :: BSL.ByteString -> MatchBody
bodyEquals body = MatchBody $ \_ body' ->
  (concat [
      "expected \""
      , BSLC8.unpack body
      , "\" got \""
      , BSLC8.unpack body'
      , "\""]) <$ guard (body /= body')

bodyStartsWith :: BSL.ByteString -> MatchBody
bodyStartsWith body = MatchBody $ \_ body' -> let body'' = BSL.take (BSL.length body) body' in
  (concat [
      "expected \""
      , BSLC8.unpack body
      , "...\" got \""
      , BSLC8.unpack body''
      , "...\""]) <$ guard (body /= body'')

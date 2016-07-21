{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

{-|
  Module:      Servant.Server.Experimental.Auth.Cookie.Internal
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental


  = Description
  Internals of `Servant.Server.Experimental.Auth.Cookie`.
-}

module Servant.Server.Experimental.Auth.HMAC.Internal where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.Maybe (isNothing, fromJust)
import Data.Serialize (Serialize, put, get)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Typeable
import GHC.TypeLits (Symbol)
import Network.HTTP.Types.Header (hWWWAuthenticate, hAuthorization)
import Network.Wai (Request, requestHeaders)
import Prelude hiding (takeWhile)
import Servant (addHeader, Proxy(..))
import Servant (throwError)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.ResponseHeaders (AddHeader)
import Servant.Server (ServantErr(..), err401, err403, errBody, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS (length, splitAt, concat, pack)
import qualified Data.ByteString.Base64 as Base64 (encode, decode)
import qualified Data.ByteString.Char8 as BS8


-- | A type family that maps user-defined account type to
--   AuthServerData. This should be instantiated as the following:
-- @
--   type instance AuthHmacAccount = UserDefinedType
-- @
type family AuthHmacAccount

-- | A type family that maps user-defined session type to
--   AuthServerData. This should be instantiated as the following:
-- @
--   type instance AuthHmacSession = UserDefinedType
-- @
type family AuthHmacSession

type AuthHmacData = (AuthHmacAccount, AuthHmacSession)
type instance AuthServerData (AuthProtect "hmac-auth") = AuthHmacData


type Realm = ByteString

-- | Options that determine authentication mechanisms.
data Settings where
  Settings :: {
    getSession :: AuthHmacAccount -> IO (Maybe AuthHmacSession)
  , maxAge     :: Int

  , errorMessage :: Maybe String
    -- ^ Message to show in response when the request is invalid.
    --   If `Nothing`, default values will be used.

  } -> Settings


-- | TODO
defaultSettings :: Settings
defaultSettings = Settings {
    getSession = undefined
  , maxAge = 300
  , errorMessage = Nothing
  }

data AuthHmacException
  = NotAuthoirized
  | NotAccepted ByteString
    deriving (Eq, Show, Typeable)

instance (Exception AuthHmacException)


parseAuthentication :: Settings -> ByteString -> Maybe (Realm, ByteString, AuthHmacAccount, UTCTime)
parseAuthentication (Settings {..}) header = either
  (const Nothing)
  getAuthData
  (parseOnly parseHeader header) where

    parseHeader :: Parser [(ByteString, ByteString)]
    parseHeader = do
      authScheme <- string "HMAC"
      _          <- takeWhile1 isLWS
      realm      <- param (Just "realm")
      authParams <- many ((char ',') *> (param Nothing))
      return $ realm:authParams

    isLWS c = (c == 9 || c == 32) -- tab or space
    isNotQuote = (/= 34)          -- any but quote

    token = takeWhile1 (inClass "a-zA-Z_-")

    param = param' . (maybe token string) where
      param' parseName = do
        name  <- parseName
        _     <- (takeWhile isLWS) *> (char '=') *> (takeWhile isLWS)
        value <- (char '"') *> (takeWhile isNotQuote) <* (char '"')
        return (name, value)

    -- TODO CI-lookup
    getAuthData :: [(ByteString, ByteString)] -> Maybe (Realm, ByteString, AuthHmacAccount, UTCTime)
    getAuthData params = do
      realm     <- flip lookup params "realm"
      hash      <- flip lookup params "hash"
      accountId <- flip lookup params "id"
      timestamp <- flip lookup params "timestamp"

      return $ undefined -- TODO


checkHash :: Settings -> ByteString -> Request -> AuthHmacAccount -> UTCTime -> Bool
checkHash (Settings {..}) _ _ _ _ = False -- TODO


-- | HMAC handler
defaultAuthHandler :: Settings -> AuthHandler Request AuthHmacData
defaultAuthHandler settings@(Settings {..}) = mkAuthHandler handler where

  handler :: Request -> Handler AuthHmacData
  handler req = do
    -- TODO catch exceptions
    result <- handler' req
    return result

  handler' :: (MonadThrow m, MonadIO m) => Request -> m AuthHmacData
  handler' req = do
    -- TODO CI-lookup
    let authHeader = lookup hAuthorization (requestHeaders req)
    when (isNothing authHeader) $ throwM NotAuthoirized

    let authData = parseAuthentication settings $ fromJust authHeader
    when (isNothing authData) $ throwM (NotAccepted "bad header")

    let (realm, reqHash, account, timestamp) = fromJust authData

    -- TODO check realm

    let expirationTime = addUTCTime (fromIntegral maxAge) timestamp
    isExpired <- liftIO $ (expirationTime >) <$> getCurrentTime
    when (isExpired) $ throwM (NotAccepted "expired request")

    session <- liftIO $ getSession account
    when (isNothing session) $ throwM (NotAccepted "account not found")

    let isCorrect = checkHash settings reqHash req account timestamp
    when (not isCorrect) $ throwM (NotAccepted "bad hash")

    return (account, fromJust session)

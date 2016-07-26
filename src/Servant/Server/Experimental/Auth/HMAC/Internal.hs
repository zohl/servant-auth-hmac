{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE PolyKinds            #-}

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
import Control.Monad.Catch (MonadThrow, throwM, catch)
import Control.Monad.IO.Class
import Crypto.Hash (HashAlgorithm(..))
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.CaseInsensitive (CI)
import Data.Default
import Data.Maybe (isNothing, fromJust)
import Data.Proxy
import Data.Serialize (Serialize, put, get)
import Data.String
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable
import Debug.Trace
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
import qualified Data.ByteString as BS (length, splitAt, concat, pack, unpack)
import qualified Data.ByteString.Base64 as Base64 (encode, decode)
import qualified Data.ByteString.Char8  as BSC8
import qualified Data.CaseInsensitive as CI (mk)

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

type instance AuthServerData (AuthProtect ("hmac-auth")) = AuthHmacData


-- | Options that determine authentication mechanisms.
data AuthHmacSettings where
  AuthHmacSettings :: (HashAlgorithm h) => {
    ahsGetSession      :: AuthHmacAccount -> IO (Maybe AuthHmacSession)
  , ahsGetSessionToken :: AuthHmacSession -> Maybe ByteString
  , ahsMaxAge          :: NominalDiffTime
  , ahsRealm           :: Maybe ByteString
  , ahsHashAlgorithm   :: Proxy h
  } -> AuthHmacSettings


instance Default AuthHmacSettings
  where def = AuthHmacSettings {
      ahsGetSession = undefined
    , ahsGetSessionToken = undefined
    , ahsMaxAge = fromIntegral (10 * 60 :: Integer) -- 10 minutes
    , ahsRealm = def
    , ahsHashAlgorithm = Proxy :: Proxy SHA256
    }


data AuthHmacException
  = NotAuthoirized
    -- ^ Thrown when there is no Authorization header in the request.
  | BadAuthorizationHeader ByteString
    -- ^ Thrown when failed to parse Authorization header. Argument of this
    -- constructor: actual header.
  | AuthorizationParameterNotFound ByteString
    -- ^ Thrown when there is missing mandatory parameter in
    -- Authorization header. Argument of this constructor: missing parameter name.
  | RequestExpired UTCTime UTCTime
    -- ^ Thrown when the request has expired. Arguments of this constructor:
    -- expiration time, actual time
  | SessionNotFound String
    -- ^ Thrown when 'ahsGetSession' returns 'Nothing'. Argument of this
    -- constructor: string representation of the account.
  | IncorrectHash ByteString ByteString
    -- ^ Thrown when hash in the header and hash or the request differ.
    -- Arguments of this constructor: expected hash, actual hash.
    deriving (Eq, Show, Typeable)

instance (Exception AuthHmacException)


parseAuthentication :: (MonadThrow m, IsString AuthHmacAccount)
  => AuthHmacSettings
  -> ByteString
  -> m (ByteString, AuthHmacAccount, UTCTime)

parseAuthentication AuthHmacSettings {..} header = either
  (\_ -> throwM $ BadAuthorizationHeader header)
  getAuthData
  (parseOnly parseHeader header) where

    parseHeader :: Parser [(ByteString, ByteString)]
    parseHeader = do
      authScheme <- stringCI "HMAC"
      _          <- takeWhile1 isLWS
      authParams <- param `sepBy` (char ',')
      return $ authParams

    isLWS c = (c == 9 || c == 32) -- tab or space
    isNotQuote = (/= 34)          -- any but quote

    token = takeWhile1 (inClass "a-zA-Z_-")

    param = do
      name  <- token
      _     <- (takeWhile isLWS) *> (char '=') *> (takeWhile isLWS)
      value <- (char '"') *> (takeWhile isNotQuote) <* (char '"')
      return (name, value)

    getAuthData :: (MonadThrow m)
      => [(ByteString, ByteString)]
      -> m (ByteString, AuthHmacAccount, UTCTime)

    getAuthData params = do
      let getParam s = maybe (throwM (AuthorizationParameterNotFound s)) return (lookup s params)
      [hash, accountId, timestamp] <- sequence $ map getParam ["hash", "id", "timestamp"]

      return (
          hash
        , fromString . BSC8.unpack $ accountId
        , posixSecondsToUTCTime . fromIntegral $ (read . BSC8.unpack $ timestamp :: Int))


 -- | Applies 'H.hmac' algorithm to given data.
sign :: forall h. HashAlgorithm h
  => Proxy h           -- ^ The hash algorithm to use
  -> ByteString        -- ^ The key
  -> ByteString        -- ^ The message
  -> ByteString
sign Proxy key msg = BA.convert (H.hmac key msg :: HMAC h)


getRequestHash
  :: AuthHmacSettings
  -> ByteString        -- ^ Token
  -> Request
  -> AuthHmacAccount
  -> UTCTime
  -> ByteString

getRequestHash AuthHmacSettings {..} token req account timestamp = sign token $ BS.concat [
  -- TODO
  ]



-- | HMAC handler
defaultAuthHandler :: (Show AuthHmacAccount, IsString AuthHmacAccount)
  => AuthHmacSettings
  -> AuthHandler Request AuthHmacData

defaultAuthHandler settings@(AuthHmacSettings {..}) = mkAuthHandler handler where

  handler :: Request -> Handler AuthHmacData
  handler req = catch (handler' req) $ \(ex :: AuthHmacException) -> throwError $ case ex of
    NotAuthoirized -> err401 {
        errHeaders = [(
            hWWWAuthenticate
          , maybe "HMAC" (\realm -> BS.concat ["HMAC realm=\"", realm, "\""]) ahsRealm)]
      }
    _ -> err403


  handler' :: (MonadThrow m, MonadIO m) => Request -> m AuthHmacData
  handler' req = do

    authHeader <- maybe (throwM NotAuthoirized) return $
      lookup (CI.mk hAuthorization) $ map (\(k, v) -> (CI.mk k, v)) (requestHeaders req)

    (reqHash, account, timestamp) <- parseAuthentication settings $ authHeader

    currentTime <- liftIO getCurrentTime

    let expirationTime = addUTCTime ahsMaxAge timestamp
    when (expirationTime > currentTime) $ throwM (RequestExpired expirationTime currentTime)

    session <- liftIO $ ahsGetSession account
    when (isNothing session) $ throwM (SessionNotFound (show account))

    let reqHash' = getRequestHash settings req account timestamp
    when (reqHash /= reqHash') $ throwM (IncorrectHash reqHash reqHash')

    return (account, fromJust session)

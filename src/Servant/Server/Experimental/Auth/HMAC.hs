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
  Module:      Servant.Server.Experimental.Auth.HMAC
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental


  = Description
  Authentication via hashed message code (HMAC) based on RFC1945.
-}

module Servant.Server.Experimental.Auth.HMAC (
    AuthHmacAccount
  , AuthHmacToken
  , AuthHmacSettings(..)
  , defaultAuthHandler

  , AuthHmacException(..)
  , parseAuthorization
  , getRequestHash
  ) where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM, catch)
import Control.Monad.IO.Class
import Crypto.Hash (HashAlgorithm(..))
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, HMAC)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char, stringCI)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.CaseInsensitive (CI(..))
import Data.Default
import Data.List (sort)
import Data.Maybe (isNothing, fromJust)
import Data.Proxy
import Data.Serialize (Serialize, put, get)
import Data.String.Class (ConvStrictByteString(..))
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable
import Debug.Trace
import GHC.TypeLits (Symbol)
import Network.HTTP.Types.Header (Header, HeaderName, hWWWAuthenticate, hAuthorization)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Request(..), requestHeaders)
import Prelude hiding (takeWhile)
import Servant (addHeader, Proxy(..))
import Servant (throwError)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.ResponseHeaders (AddHeader)
import Servant.Server (ServantErr(..), err401, err403, errBody, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64 (encode, decode)
import qualified Data.ByteString.Char8  as BSC8
import qualified Data.CaseInsensitive as CI (mk)


-- | A type family that maps user-defined account type to
--   AuthServerData. This should be instantiated as the following:
-- @
--   type instance AuthHmacAccount = UserDefinedType
-- @
type family AuthHmacAccount

-- | A type family that maps user-defined token type to
--   AuthServerData. This should be instantiated as the following:
-- @
--   type instance AuthHmacToken = UserDefinedType
-- @
type family AuthHmacToken


type AuthHmacData = (AuthHmacAccount, AuthHmacToken)

type instance AuthServerData (AuthProtect ("hmac-auth")) = AuthHmacData


-- | Options that determine authentication mechanisms.
data AuthHmacSettings where
  AuthHmacSettings :: (HashAlgorithm h) => {
    ahsGetToken      :: AuthHmacAccount -> IO (Maybe AuthHmacToken)
  , ahsMaxAge        :: NominalDiffTime
  , ahsRealm         :: Maybe ByteString
  , ahsHashAlgorithm :: Proxy h
  , ahsHeaderFilter  :: HeaderName -> Bool
  } -> AuthHmacSettings


instance Default AuthHmacSettings
  where def = AuthHmacSettings {
      ahsGetToken = undefined
    , ahsMaxAge = fromIntegral (10 * 60 :: Integer) -- 10 minutes
    , ahsRealm = def
    , ahsHashAlgorithm = Proxy :: Proxy SHA256
    , ahsHeaderFilter = ( == "Content-Type")
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
  | TokenNotFound ByteString
    -- ^ Thrown when 'ahsGetToken' returns 'Nothing'. Argument of this
    -- constructor: string representation of the account.
  | IncorrectHash ByteString ByteString
    -- ^ Thrown when hash in the header and hash or the request differ.
    -- Arguments of this constructor: expected hash, actual hash.
    deriving (Eq, Show, Typeable)

instance (Exception AuthHmacException)


parseAuthorization :: (MonadThrow m, ConvStrictByteString AuthHmacAccount)
  => AuthHmacSettings
  -> ByteString
  -> m (ByteString, AuthHmacAccount, UTCTime)

parseAuthorization AuthHmacSettings {..} hdr = either
  (\_ -> throwM $ BadAuthorizationHeader hdr)
  getAuthData
  (parseOnly header hdr) where

    header :: Parser [(ByteString, ByteString)]
    header = do
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
        , fromStrictByteString accountId
        , posixSecondsToUTCTime . fromIntegral $ (read . BSC8.unpack $ timestamp :: Int))



sign :: forall h. HashAlgorithm h
  => Proxy h           -- ^ The hash algorithm to use
  -> ByteString        -- ^ The key
  -> ByteString        -- ^ The message
  -> ByteString
sign Proxy key msg = BA.convert (hmac key msg :: HMAC h)


getRequestHash :: (ConvStrictByteString AuthHmacAccount, ConvStrictByteString AuthHmacToken)
  => AuthHmacSettings
  -> AuthHmacToken
  -> AuthHmacAccount
  -> UTCTime
  -> ByteString        -- ^ URI
  -> Method
  -> [Header]
  -> ByteString        -- ^ Request Body
  -> ByteString

getRequestHash AuthHmacSettings {..} key account timestamp uri method headers body
  = sign ahsHashAlgorithm (toStrictByteString key) $ BS.intercalate "\n" [
      toStrictByteString account
    , BSC8.pack . show $ ((truncate . utcTimeToPOSIXSeconds $ timestamp)::Integer)
    , uri
    , method
    , normalizeHeaders $ filter (\(name, _) -> ahsHeaderFilter name) headers
    , body
    ] where

  normalizeHeaders = BS.intercalate "\n" . sort . map normalize where
    normalize (name, value) = BS.concat [foldedCase name, value]

-- | HMAC handler
defaultAuthHandler :: (ConvStrictByteString AuthHmacAccount, ConvStrictByteString AuthHmacToken)
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
    _ -> err403 {
        errBody = fromStrict $ BSC8.pack $ show ex
      }


  handler' :: (MonadThrow m, MonadIO m) => Request -> m AuthHmacData
  handler' req = do

    authHeader <- maybe (throwM NotAuthoirized) return $
      lookup (CI.mk hAuthorization) $ map (\(k, v) -> (CI.mk k, v)) (requestHeaders req)

    (reqHash, account, timestamp) <- parseAuthorization settings $ authHeader

    currentTime <- liftIO getCurrentTime

    let expirationTime = addUTCTime ahsMaxAge timestamp
    when (expirationTime < currentTime) $ throwM (RequestExpired expirationTime currentTime)

    token <- (liftIO $ ahsGetToken account)
      >>= maybe (throwM (TokenNotFound (toStrictByteString account))) return

    reqHash' <- liftIO $ Base64.encode <$> getRequestHash
      settings
      token
      account
      timestamp
      (rawPathInfo req)
      (requestMethod req)
      (requestHeaders req)
      <$> (requestBody req)

    when (reqHash /= reqHash') $ throwM (IncorrectHash reqHash reqHash')

    return (account, token)


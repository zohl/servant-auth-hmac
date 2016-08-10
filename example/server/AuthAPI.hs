{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}

module AuthAPI (
    Username
  , Password
  , Token
  , Secret
  , Storage

  , AuthAPI
  , serveAuth
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.Maybe (fromJust, isNothing)
import Data.Default
import Data.Serialize (Serialize)
import qualified Data.Text as T (unpack)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import System.Posix.Directory (getWorkingDirectory)

import Servant (
    Proxy(..), Server, (:>), (:<|>)(..)
  , Header, Headers, addHeader
  , Raw
  , Get, Post, ReqBody, Capture, FormUrlEncoded, FromFormUrlEncoded(..))

import Servant.Server (
    Context ((:.), EmptyContext)
  , serveWithContext
  , serve)

import Servant                          (throwError)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.API.ContentTypes         (Accept(..), MimeRender(..), JSON)
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Experimental.Auth.HMAC
import Servant.Server (err401, err403, err404)
import Servant.Utils.StaticFiles (serveDirectory)

import Network.Wai              (Application, Request)
import Network.Wai.Handler.Warp (run)

import GHC.Generics

import Network.HTTP.Media ((//), (/:))

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)

import Data.Map (Map)
import qualified Data.Map as Map

import System.Random
import Control.Monad.Trans.Except (ExceptT)
import Servant.Server (ServantErr)

import Data.String.Class (ConvStrictByteString(..))
import Debug.Trace


type Username = String
type Password = String
type Token = String
type Secret = String
type Storage = IORef (Map Username Token)


type instance AuthHmacAccount = Username
type instance AuthHmacToken = Token


data LoginArgs = LoginArgs {
    username :: String
  , password :: String
  } deriving Generic

instance FromJSON LoginArgs


type AuthAPI = "login" :> ReqBody '[JSON] LoginArgs :> Post '[JSON] String
          :<|> "secret" :> Capture "username" Username
            :> AuthProtect "hmac-auth" :> Get '[JSON] String


users :: [(Username, (Password, Secret))]
users = [
    ("mr_foo", ("password1", "War is Peace"))
  , ("mr_bar", ("letmein"  , "Freedom is Slavery"))
  , ("mr_baz", ("baseball" , "Ignorance is Strength"))
  ]


serveAuth :: Storage -> AuthHmacSettings -> Server AuthAPI
serveAuth storage settings = serveLogin :<|> serveSecret where

  serveLogin args = serve' where

    serve' = case isValidUser of
      True  -> liftIO $ getToken
      False -> throwError err403

    isValidUser = maybe False
      (\(password', _) -> password' == (password args))
      (lookup (username args) users)

    getToken :: IO String
    getToken = (maybe mkToken return) =<< ((Map.lookup (username args)) <$> (readIORef storage))

    mkToken :: IO String
    mkToken = do
      token <- (take 16 . randomRs ('A', 'Z')) <$> getStdGen
      modifyIORef storage (Map.insert (username args) token)
      return token

  serveSecret :: Username -> (Username, Token) -> ExceptT ServantErr IO String
  serveSecret username' (username'', _) = do
    when (username' /= username'') $ throwError err403 -- User can request only his own secret
    maybe (throwError err404) (\(_, secret') -> return secret') (lookup username' users)


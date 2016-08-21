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
  , LoginArgs(..)

  , AuthAPI
  , serveAuth
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import Data.Aeson.Types (Options, genericParseJSON, genericToJSON, fieldLabelModifier)
import Data.Char (isUpper, toLower)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Map (Map)
import GHC.Generics
import Servant (Server, (:>), (:<|>)(..), Get, Post, ReqBody, Capture)
import Servant (throwError)
import Servant.API.ContentTypes (JSON)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (ServantErr)
import Servant.Server (err403, err404)
import Servant.Server.Experimental.Auth.HMAC
import System.Random
import qualified Data.Map as Map


type Username = String
type Password = String
type Token = String
type Secret = String
type Storage = IORef (Map Username Token)

type instance AuthHmacAccount = Username
type instance AuthHmacToken = Token

data LoginArgs = LoginArgs {
    laUsername :: String
  , laPassword :: String
  } deriving Generic

instance FromJSON LoginArgs where
  parseJSON = genericParseJSON dropPrefixOptions

instance ToJSON LoginArgs where
  toJSON = genericToJSON dropPrefixOptions

dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix }


type AuthAPI = "login" :> ReqBody '[JSON] LoginArgs :> Post '[JSON] String
          :<|> "secret" :> Capture "username" Username
            :> AuthProtect "hmac-auth" :> Get '[JSON] String

users :: [(Username, (Password, Secret))]
users = [
    ("mr_foo", ("password1", "War is Peace"))
  , ("mr_bar", ("letmein"  , "Freedom is Slavery"))
  , ("mr_baz", ("baseball" , "Ignorance is Strength"))
  ]

serveAuth :: Storage -> Server AuthAPI
serveAuth storage = serveLogin :<|> serveSecret where

  serveLogin (LoginArgs {..}) = serve' where

    serve' = case isValidUser of
      True  -> liftIO $ getToken
      False -> throwError err403

    isValidUser = maybe False
      (\(password', _) -> password' == laPassword)
      (lookup laUsername users)

    getToken :: IO String
    getToken = (maybe mkToken return) =<< ((Map.lookup laUsername) <$> (readIORef storage))

    mkToken :: IO String
    mkToken = do
      token <- (take 16 . randomRs ('A', 'Z')) <$> getStdGen
      modifyIORef storage (Map.insert laUsername token)
      return token

  serveSecret :: Username -> (Username, Token) -> ExceptT ServantErr IO String
  serveSecret username' (username'', _) = do
    when (username' /= username'') $ throwError err403 -- User can request only his own secret
    maybe (throwError err404) (\(_, secret') -> return secret') (lookup username' users)



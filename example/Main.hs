{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}


import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.Maybe (fromJust, isNothing)
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



type Username = String
type Password = String
type Token = String
type Secret = String
type Session = (Password, Maybe Token, Secret)

type Storage = IORef (Map Username Session)


data HTML

instance Accept HTML where
   contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
   mimeRender _ x = fromStrict $ x


data LoginArgs = LoginArgs {
    username :: String
  , password :: String
  } deriving Generic;

instance FromJSON LoginArgs



type instance AuthHmacAccount = Username
type instance AuthHmacSession = Session

type ExampleAPI = Get '[HTML] ByteString
             :<|> "static" :> Raw
             :<|> "api" :> "templates" :> Get '[JSON] [String]
             :<|> "api" :> "templates" :> Capture "name" String :> Get '[HTML] ByteString
             :<|> "api" :> "login" :> ReqBody '[JSON] LoginArgs :> Post '[JSON] String
             :<|> "api" :> "secret" :> Capture "username" Username
                                      :> AuthProtect "hmac-auth" :> Get '[JSON] String


server :: FilePath -> Storage -> Settings -> Server ExampleAPI
server root storage settings = serveIndex
         :<|> serveStatic
         :<|> serveTemplates
         :<|> serveTemplate
         :<|> serveLogin
         :<|> serveSecret where

  serveIndex = return $ render indexPage

  serveStatic = serveDirectory root

  templates = [
      ("home"   , homePage)
    , ("login"  , loginPage)
    , ("private", privatePage)
    ]

  render = toStrict . renderHtml

  serveTemplates = return $ map fst templates

  serveTemplate name = return . render $ (maybe defaultPage id) (lookup name templates)

  serveLogin args = getUser >>= getToken where
    getUser = do
      result <- liftIO $ (Map.lookup (username args) <$> (readIORef storage))
      case result of
        Just (password', token, _) -> case (password' == (password args)) of
          True -> return token
          False -> throwError err403
        Nothing -> throwError err403

    getToken (Just token) = return token
    getToken Nothing = do
      token <- liftIO $ (take 16 . randomRs ('A', 'Z')) <$> getStdGen
      liftIO $ modifyIORef
        storage
        (Map.adjust (\(password', _, secret) -> (password', Just token, secret)) (username args))

      return token

  serveSecret :: Username -> (Username, Session) -> ExceptT ServantErr IO String
  serveSecret username' (username'', (_, _, secret)) = case (username' == username'') of
    True  -> return secret
    False -> throwError err403 -- User can request only his own secret


app :: FilePath -> Storage -> Settings -> Application
app root storage settings = serveWithContext
  (Proxy :: Proxy ExampleAPI)
  ((defaultAuthHandler settings) :. EmptyContext)
  (server root storage settings)


main :: IO ()
main = do
  root <- (++ "/example/client/result/static") <$> getWorkingDirectory

  storage <- newIORef $ Map.fromList [
      ("mr_foo", ("password1", Nothing, "War is Peace"))
    , ("mr_bar", ("letmein"  , Nothing, "Freedom is Slavery"))
    , ("mr_baz", ("baseball" , Nothing, "Ignorance is Strength"))
    ]

  let authSettings = ($ defaultSettings) $ \(Settings {..}) -> Settings {
      getSession = (\username -> (Map.lookup username) <$> (readIORef storage))
    , ..
    }

  run 8080 (app root storage authSettings)


indexPage :: H.Html
indexPage = H.docTypeHtml $ do
  H.head $ do
    H.script ! A.src "static/app.js" ! A.type_ "application/javascript;version=1.8" $ ""
  H.body $ do
    H.div ! A.class_ "app" $ "Loading..."


homePage :: H.Html
homePage = do
  H.p "This is an example of using servant-auth-hmac library."
  H.p "Use login page to get access to the private page."


loginPage :: H.Html
loginPage = do
  H.form ! A.method "post" ! A.action "/api/login" $ do
    H.table $ do
      H.tr $ do
       H.td $ "username:"
       H.td $ H.input ! A.type_ "text" ! A.name "username"
      H.tr $ do
       H.td $ "password:"
       H.td $ H.input ! A.type_ "password" ! A.name "password"
    H.input ! A.type_ "submit"
  H.p ! A.class_ "feedback" $ ""


privatePage :: H.Html
privatePage = do
  H.p $ H.b "username: " >> "{{username}}"
  H.p $ H.b "token: "    >> "{{token}}"
  H.p $ H.b "secret: "   >> "{{secret}}"


defaultPage :: H.Html
defaultPage = H.p "not found"

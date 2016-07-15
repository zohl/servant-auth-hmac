{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


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


type Username = String
type Password = String
type Token = String
type Secret = String

type Storage = IORef (Map Username (Password, Maybe Token, Secret))


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


type ExampleAPI = Get '[HTML] ByteString
             :<|> "static" :> Raw
             :<|> "api" :> "templates" :> Get '[JSON] [String]
             :<|> "api" :> "templates" :> Capture "name" String :> Get '[HTML] ByteString
             :<|> "api" :> "login" :> ReqBody '[JSON] LoginArgs :> Post '[JSON] String
             :<|> "api" :> "secret" :> Capture "username" String :> Get '[JSON] String


server :: FilePath -> Storage -> Server ExampleAPI
server root storage = serveIndex
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

  -- TODO check credentials
  serveSecret username' = do
    result <- liftIO $ (Map.lookup username' <$> readIORef storage)
    case result of
      Nothing -> throwError err404
      Just (_, _, secret) -> return secret


app :: FilePath -> Storage -> Application
app root storage = serve (Proxy :: Proxy ExampleAPI) (server root storage)


main :: IO ()
main = do
  root <- (++ "/example/client/result/static") <$> getWorkingDirectory
  storage <- newIORef $ Map.fromList [
      ("mr_foo", ("password1", Nothing, "War is Peace"))
    , ("mr_bar", ("letmein"  , Nothing, "Freedom is Slavery"))
    , ("mr_baz", ("baseball" , Nothing, "Ignorance is Strength"))
    ]

  run 8080 (app root storage)


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

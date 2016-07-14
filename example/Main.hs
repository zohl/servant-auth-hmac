{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import Control.Monad (when)

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

import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.API.ContentTypes         (Accept(..), MimeRender(..), JSON)
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Experimental.Auth.HMAC

import Network.Wai              (Application, Request)
import Network.Wai.Handler.Warp (run)

import GHC.Generics

import Network.HTTP.Media ((//), (/:))

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


import Servant.Utils.StaticFiles (serveDirectory)


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

server :: FilePath -> Server ExampleAPI
server root = serveIndex
         :<|> serveStatic
         :<|> serveTemplates
         :<|> serveTemplate
         :<|> serveLogin where

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

  serveLogin args = return $ "TODO"


app :: FilePath -> Application
app root = serve (Proxy :: Proxy ExampleAPI) (server root)


main :: IO ()
main = do
  root <- (++ "/example/client/result/static") <$> getWorkingDirectory
  run 8080 (app root)


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


privatePage :: H.Html
privatePage = do
  H.p $ H.b "username: " >> "{{username}}"
  H.p $ H.b "token: "    >> "{{token}}"
  H.p $ H.b "secret: "   >> "{{secret}}"


defaultPage :: H.Html
defaultPage = H.p "not found"

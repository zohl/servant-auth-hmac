{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}

import AuthAPI
import Data.Default
import Data.IORef (newIORef, readIORef)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(..), Server, (:>), (:<|>)(..), Raw, Get, Capture)
import Servant.API.ContentTypes (JSON)
import Servant.HTML.Blaze
import Servant.Server (Context ((:.), EmptyContext), serveWithContext)
import Servant.Server.Experimental.Auth.HMAC
import Servant.Utils.StaticFiles (serveDirectory)
import System.Posix.Directory (getWorkingDirectory)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 (Markup)
import qualified Data.Map as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


type ExampleAPI = Get '[HTML] Markup
             :<|> "static" :> Raw
             :<|> "api" :> "templates" :> Get '[JSON] [String]
             :<|> "api" :> "templates" :> Capture "name" String :> Get '[HTML] Markup
             :<|> "api" :> AuthAPI


server :: FilePath -> Storage -> AuthHmacSettings -> Server ExampleAPI
server root storage settings = serveIndex
                          :<|> serveStatic
                          :<|> serveTemplates
                          :<|> serveTemplate
                          :<|> serveAuth storage settings where

  serveIndex = return indexPage
  serveStatic = serveDirectory root
  serveTemplates = return $ map fst templates
  serveTemplate name = return $ (maybe defaultPage id) (lookup name templates)

  templates = [
      ("home"   , homePage)
    , ("login"  , loginPage)
    , ("private", privatePage)
    ]


app :: FilePath -> Storage -> AuthHmacSettings -> Application
app root storage settings = serveWithContext
  (Proxy :: Proxy ExampleAPI)
  ((defaultAuthHandler settings) :. EmptyContext)
  (server root storage settings)

main :: IO ()
main = do
  root <- (++ "/example/client/result/static") <$> getWorkingDirectory

  storage <- newIORef $ Map.empty

  let authSettings = ($ def) $ \(AuthHmacSettings {..}) -> AuthHmacSettings {
      ahsGetToken = \username -> (Map.lookup username) <$> (readIORef storage)
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

{-|
  Module:      Servant.Server.Experimental.HMAC.Cookie
  Copyright:   (c) 2016 Al Zohali
  License:     GPL3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description

  Authentication via hashed message code (HMAC) based on RFC1945.
-}

module Servant.Server.Experimental.Auth.HMAC (
    AuthHmacAccount
  , AuthHmacSession
  , AuthHmacData
  , Settings(..)
  , defaultSettings
  , defaultAuthHandler
  ) where

import Servant.Server.Experimental.Auth.HMAC.Internal

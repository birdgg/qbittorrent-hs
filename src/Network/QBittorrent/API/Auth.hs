-- | Auth routes for qBittorrent Web API
module Network.QBittorrent.API.Auth
  ( AuthRoutes (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Form (LoginForm)
import Servant.API

-- | Auth routes
data AuthRoutes mode = AuthRoutes
  { login
      :: mode
        :- "login"
          :> ReqBody '[FormUrlEncoded] LoginForm
          :> Post '[PlainText] Text
  , logout
      :: mode
        :- "logout"
          :> Post '[PlainText] NoContent
  }
  deriving stock (Generic)

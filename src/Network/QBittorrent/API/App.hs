-- | App routes for qBittorrent Web API
module Network.QBittorrent.API.App
  ( AppRoutes (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Form (PreferencesForm)
import Servant.API

-- | App routes
data AppRoutes mode = AppRoutes
  { defaultSavePath
      :: mode
        :- "defaultSavePath"
          :> Get '[PlainText] Text
  , setPreferences
      :: mode
        :- "setPreferences"
          :> ReqBody '[FormUrlEncoded] PreferencesForm
          :> Post '[PlainText] NoContent
  }
  deriving stock (Generic)

-- | App routes for qBittorrent Web API
module Network.QBittorrent.API.App
  ( AppRoutes (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.App (BuildInfo, Preferences)
import Network.QBittorrent.Types.Form (PreferencesForm)
import Servant.API

-- | App routes
data AppRoutes mode = AppRoutes
  { version
      :: mode
        :- "version"
          :> Get '[PlainText] Text
  , webapiVersion
      :: mode
        :- "webapiVersion"
          :> Get '[PlainText] Text
  , buildInfo
      :: mode
        :- "buildInfo"
          :> Get '[JSON] BuildInfo
  , shutdown
      :: mode
        :- "shutdown"
          :> Get '[PlainText] NoContent
  , preferences
      :: mode
        :- "preferences"
          :> Get '[JSON] Preferences
  , setPreferences
      :: mode
        :- "setPreferences"
          :> ReqBody '[FormUrlEncoded] PreferencesForm
          :> Post '[PlainText] NoContent
  , defaultSavePath
      :: mode
        :- "defaultSavePath"
          :> Get '[PlainText] Text
  , -- | Get list of network interfaces
    networkInterfaceList
      :: mode
        :- "networkInterfaceList"
          :> Get '[JSON] [Text]
  , -- | Get addresses for a network interface
    networkInterfaceAddressList
      :: mode
        :- "networkInterfaceAddressList"
          :> QueryParam' '[Required, Strict] "iface" Text
          :> Get '[JSON] [Text]
  }
  deriving stock (Generic)

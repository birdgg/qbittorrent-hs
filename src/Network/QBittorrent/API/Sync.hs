-- | Sync routes for qBittorrent Web API
module Network.QBittorrent.API.Sync
  ( SyncRoutes (..)
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Sync (SyncMainData)
import Servant.API

-- | Sync routes
data SyncRoutes mode = SyncRoutes
  { maindata
      :: mode
        :- "maindata"
          :> QueryParam' '[Required, Strict] "rid" Int64
          :> Get '[JSON] SyncMainData
  }
  deriving stock (Generic)

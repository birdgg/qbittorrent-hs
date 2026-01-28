-- | Sync routes for qBittorrent Web API
module Network.QBittorrent.API.Sync
  ( SyncRoutes (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Sync (SyncMainData, SyncTorrentPeers)
import Servant.API

-- | Sync routes
data SyncRoutes mode = SyncRoutes
  { maindata
      :: mode
        :- "maindata"
          :> QueryParam' '[Required, Strict] "rid" Int64
          :> Get '[JSON] SyncMainData
  , torrentPeers
      :: mode
        :- "torrentPeers"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> QueryParam "rid" Int64
          :> Get '[JSON] SyncTorrentPeers
  }
  deriving stock (Generic)

-- | Log routes for qBittorrent Web API
module Network.QBittorrent.API.Log
  ( LogRoutes (..)
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Log (LogEntry, PeerLogEntry)
import Servant.API

-- | Log routes
data LogRoutes mode = LogRoutes
  { -- | Get log entries
    --
    -- Query parameters:
    -- - normal: Include normal messages (default: true)
    -- - info: Include info messages (default: true)
    -- - warning: Include warning messages (default: true)
    -- - critical: Include critical messages (default: true)
    -- - last_known_id: Exclude messages with id <= last_known_id (default: -1)
    main
      :: mode
        :- "main"
          :> QueryFlag "normal"
          :> QueryFlag "info"
          :> QueryFlag "warning"
          :> QueryFlag "critical"
          :> QueryParam "last_known_id" Int64
          :> Get '[JSON] [LogEntry]
  , -- | Get peer log entries
    --
    -- Query parameter:
    -- - last_known_id: Exclude entries with id <= last_known_id (default: -1)
    peers
      :: mode
        :- "peers"
          :> QueryParam "last_known_id" Int64
          :> Get '[JSON] [PeerLogEntry]
  }
  deriving stock (Generic)

-- | qBittorrent Web API definition for servant-client
module Network.QBittorrent.API
  ( -- * API Types
    QBittorrentAPI
  , QBittorrentRoutes (..)

    -- * Route Definitions (re-exported)
  , module Network.QBittorrent.API.Auth
  , module Network.QBittorrent.API.Torrents
  , module Network.QBittorrent.API.App
  , module Network.QBittorrent.API.Sync

    -- * Configuration
  , mkBaseUrl
  ) where

import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.QBittorrent.API.App
import Network.QBittorrent.API.Auth
import Network.QBittorrent.API.Sync
import Network.QBittorrent.API.Torrents
import Network.QBittorrent.Types (QBConfig (..))
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))

-- | qBittorrent Web API v2
type QBittorrentAPI = "api" :> "v2" :> NamedRoutes QBittorrentRoutes

-- | Top-level routes
data QBittorrentRoutes mode = QBittorrentRoutes
  { auth :: mode :- "auth" :> NamedRoutes AuthRoutes
  , torrents :: mode :- "torrents" :> NamedRoutes TorrentsRoutes
  , app :: mode :- "app" :> NamedRoutes AppRoutes
  , sync :: mode :- "sync" :> NamedRoutes SyncRoutes
  }
  deriving stock (Generic)

-- | Build base URL from config
--
-- Note: The client uses QBittorrentRoutes directly (not QBittorrentAPI),
-- so we need to include the /api/v2 prefix in the base URL path.
mkBaseUrl :: QBConfig -> BaseUrl
mkBaseUrl cfg =
  BaseUrl
    { baseUrlScheme = if cfg.useTLS then Https else Http
    , baseUrlHost = T.unpack cfg.host
    , baseUrlPort = cfg.port
    , baseUrlPath = "/api/v2"
    }

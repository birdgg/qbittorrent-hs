-- | qBittorrent Web API definition for servant-client
module Network.QBittorrent.API
  ( -- * API Types
    QBittorrentAPI
  , QBittorrentRoutes (..)
  , AuthRoutes (..)
  , TorrentsRoutes (..)
  , AppRoutes (..)
  , SyncRoutes (..)

    -- * Configuration
  , mkBaseUrl
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.QBittorrent.Types
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

-- | Torrents routes
data TorrentsRoutes mode = TorrentsRoutes
  { add
      :: mode
        :- "add"
          :> ReqBody '[FormUrlEncoded] AddTorrentRequest
          :> Post '[PlainText] Text
  , info
      :: mode
        :- "info"
          :> QueryParam "filter" Text
          :> QueryParam "category" Text
          :> QueryParam "tag" Text
          :> QueryParam "hashes" Text
          :> Get '[JSON] [TorrentInfo]
  , files
      :: mode
        :- "files"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] [TorrentFile]
  , pause
      :: mode
        :- "pause"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , resume
      :: mode
        :- "resume"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , delete
      :: mode
        :- "delete"
          :> ReqBody '[FormUrlEncoded] DeleteTorrentsForm
          :> Post '[PlainText] NoContent
  , addTags
      :: mode
        :- "addTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent
  , removeTags
      :: mode
        :- "removeTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent
  , renameFile
      :: mode
        :- "renameFile"
          :> ReqBody '[FormUrlEncoded] RenameFileForm
          :> Post '[PlainText] NoContent
  , renameFolder
      :: mode
        :- "renameFolder"
          :> ReqBody '[FormUrlEncoded] RenameFolderForm
          :> Post '[PlainText] NoContent
  , setLocation
      :: mode
        :- "setLocation"
          :> ReqBody '[FormUrlEncoded] SetLocationForm
          :> Post '[PlainText] NoContent
  }
  deriving stock (Generic)

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

-- | Sync routes
data SyncRoutes mode = SyncRoutes
  { maindata
      :: mode
        :- "maindata"
          :> QueryParam' '[Required, Strict] "rid" Int64
          :> Get '[JSON] SyncMainData
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

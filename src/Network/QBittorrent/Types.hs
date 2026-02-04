-- | Core types for qBittorrent API client
module Network.QBittorrent.Types
  ( -- * Configuration
    QBConfig (..)
  , Credentials (..)
  , defaultConfig

    -- * Errors
  , QBError (..)

    -- * Re-exports
  , module Network.QBittorrent.Types.App
  , module Network.QBittorrent.Types.Filter
  , module Network.QBittorrent.Types.Form
  , module Network.QBittorrent.Types.Log
  , module Network.QBittorrent.Types.Sync
  , module Network.QBittorrent.Types.Torrent
  , module Network.QBittorrent.Types.Transfer
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.App
import Network.QBittorrent.Types.Filter
import Network.QBittorrent.Types.Form
import Network.QBittorrent.Types.Log
import Network.QBittorrent.Types.Sync
import Network.QBittorrent.Types.Torrent
import Network.QBittorrent.Types.Transfer

-- | qBittorrent client configuration
data QBConfig = QBConfig
  { host :: Text
  , port :: Int
  , username :: Text
  , password :: Text
  , useTLS :: Bool
  }
  deriving stock (Show, Eq, Generic)

-- | Authentication credentials
data Credentials = Credentials
  { username :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Default configuration for localhost
defaultConfig :: QBConfig
defaultConfig =
  QBConfig
    { host = "localhost"
    , port = 8080
    , username = "admin"
    , password = "adminadmin"
    , useTLS = False
    }

-- | Errors that can occur during qBittorrent API operations
data QBError
  = -- | Network request failed
    NetworkError Text
  | -- | Authentication failed
    AuthError Text
  | -- | API returned an error status (status code, message)
    ApiError Int Text
  | -- | Failed to parse JSON response
    ParseError Text
  | -- | Invalid torrent provided
    InvalidTorrent Text
  deriving stock (Show, Eq)

-- | Core types for qBittorrent API client
module Network.QBittorrent.Types
  ( -- * Configuration
    QBConfig (..)
  , defaultConfig

    -- * Client
  , QBClient (..)

    -- * Errors
  , QBError (..)
  , clientErrorToQBError

    -- * Re-exports
  , module Network.QBittorrent.Types.App
  , module Network.QBittorrent.Types.Credential
  , module Network.QBittorrent.Types.Filter
  , module Network.QBittorrent.Types.Form
  , module Network.QBittorrent.Types.InfoHash
  , module Network.QBittorrent.Types.Log
  , module Network.QBittorrent.Types.Sync
  , module Network.QBittorrent.Types.Tag
  , module Network.QBittorrent.Types.Torrent
  , module Network.QBittorrent.Types.Transfer
  ) where

import Control.Concurrent.STM (TVar)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client (CookieJar)
import Servant.Client (ClientEnv, ClientError (..))
import Servant.Client.Core (ResponseF (..))
import Network.QBittorrent.Types.App
import Network.QBittorrent.Types.Credential
import Network.QBittorrent.Types.Filter
import Network.QBittorrent.Types.Form
import Network.QBittorrent.Types.InfoHash
import Network.QBittorrent.Types.Log
import Network.QBittorrent.Types.Sync
import Network.QBittorrent.Types.Tag
import Network.QBittorrent.Types.Torrent
import Network.QBittorrent.Types.Transfer

-- | qBittorrent client configuration
data QBConfig = QBConfig
  { host :: Text
  , port :: Int
  , credential :: Credential
  , useTLS :: Bool
  }
  deriving stock (Show, Eq, Generic)

-- | Default configuration for localhost
defaultConfig :: QBConfig
defaultConfig =
  QBConfig
    { host = "localhost"
    , port = 8080
    , credential = Credential "admin" "adminadmin"
    , useTLS = False
    }

-- | qBittorrent client with automatic session management
--
-- The client maintains a cookie jar internally for session persistence.
-- Create with 'initQBClient' and use with 'runQB'.
data QBClient = QBClient
  { clientEnv :: ClientEnv
  , cookieJar :: TVar CookieJar
  , config :: QBConfig
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

-- | Convert servant ClientError to QBError
clientErrorToQBError :: ClientError -> QBError
clientErrorToQBError = \case
  FailureResponse _ Response{responseStatusCode, responseBody}
    | fromEnum responseStatusCode == 403 ->
        AuthError "IP is banned for too many failed login attempts"
    | otherwise ->
        ApiError (fromEnum responseStatusCode) (T.pack $ show responseBody)
  DecodeFailure msg _ ->
    ParseError msg
  UnsupportedContentType _ _ ->
    ParseError "Unsupported content type"
  InvalidContentTypeHeader _ ->
    ParseError "Invalid content type header"
  ConnectionError ex ->
    NetworkError (T.pack $ show ex)

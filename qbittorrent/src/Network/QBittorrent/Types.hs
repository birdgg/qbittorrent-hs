-- | Core types for qBittorrent API client
module Network.QBittorrent.Types
  ( -- * Configuration
    QBConfig (..)
  , defaultConfig

    -- * Client
  , QBClient (..)

    -- * Errors
  , QBClientError (..)
  , QBResponseError (..)
  , clientErrorToQBClientError

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
import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
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

-- | API response error from qBittorrent
--
-- Represents errors returned by the qBittorrent Web API with
-- structured status code and response body information.
data QBResponseError = QBResponseError
  { statusCode :: Int
  , responseBody :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Errors that can occur during qBittorrent API operations
--
-- This type distinguishes between:
--
-- * Network-level errors from servant-client ('ServantError')
-- * API-level errors from qBittorrent ('QBApiError')
data QBClientError
  = -- | Network/HTTP error from servant-client
    ServantError ClientError
  | -- | API returned an error response
    QBApiError QBResponseError
  deriving stock (Show, Eq)

instance Exception QBClientError

-- | Convert servant ClientError to QBClientError
--
-- HTTP failure responses become 'QBApiError' with structured status code
-- and response body. All other errors (connection, decode, content type)
-- are preserved as 'ServantError'.
clientErrorToQBClientError :: ClientError -> QBClientError
clientErrorToQBClientError err = case err of
  FailureResponse _ Response{responseStatusCode, responseBody} ->
    QBApiError
      QBResponseError
        { statusCode = fromEnum responseStatusCode
        , responseBody = TL.toStrict $ TLE.decodeUtf8With lenientDecode responseBody
        }
  _ -> ServantError err

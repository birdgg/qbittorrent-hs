-- | Authentication helpers for qBittorrent client
module Network.QBittorrent.Client.Auth
  ( -- * Client
    QBClient (..)
  , newClient

    -- * Running Requests
  , runQB
  ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Network.HTTP.Client (CookieJar, Manager)
import Network.QBittorrent.API (mkBaseUrl)
import Network.QBittorrent.Types (QBConfig)
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)
import Servant.Client qualified as Servant

-- | qBittorrent client with automatic session management
--
-- The client maintains a cookie jar internally for session persistence.
-- Create with 'newClient' and use with 'runQB'.
data QBClient = QBClient
  { clientEnv :: ClientEnv
  , cookieJar :: TVar CookieJar
  , config :: QBConfig
  }

-- | Create a new qBittorrent client
--
-- The cookie jar is created and managed internally.
--
-- @
-- manager <- newManager tlsManagerSettings
-- client <- newClient manager config
-- result <- runQB client (login config)
-- @
newClient :: Manager -> QBConfig -> IO QBClient
newClient manager cfg = do
  jar <- newTVarIO mempty
  let baseEnv = Servant.mkClientEnv manager (mkBaseUrl cfg)
      env = baseEnv{Servant.cookieJar = Just jar}
  pure QBClient{clientEnv = env, cookieJar = jar, config = cfg}

-- | Run a qBittorrent API request
--
-- The session cookie is automatically maintained across requests.
runQB :: QBClient -> ClientM a -> IO (Either ClientError a)
runQB client action = runClientM action client.clientEnv

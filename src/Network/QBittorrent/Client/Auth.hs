-- | Authentication helpers for qBittorrent client
module Network.QBittorrent.Client.Auth
  ( -- * Client Environment
    mkClientEnv
  , mkClientEnvWithCookies

    -- * Cookie Management
  , newCookieJar
  ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Network.HTTP.Client (CookieJar, Manager)
import Network.QBittorrent.API (mkBaseUrl)
import Network.QBittorrent.Types (QBConfig)
import Servant.Client (ClientEnv)
import Servant.Client qualified as Servant

-- | Create a new empty cookie jar
newCookieJar :: IO (TVar CookieJar)
newCookieJar = newTVarIO mempty

-- | Create a ClientEnv without cookie persistence
--
-- Note: This is suitable for single requests but not for session-based auth.
-- For qBittorrent, use 'mkClientEnvWithCookies' instead.
mkClientEnv :: Manager -> QBConfig -> ClientEnv
mkClientEnv manager config =
  Servant.mkClientEnv manager (mkBaseUrl config)

-- | Create a ClientEnv with cookie persistence
--
-- This is required for qBittorrent authentication, which uses session cookies (SID).
-- The cookie jar maintains session state across requests.
mkClientEnvWithCookies :: Manager -> QBConfig -> TVar CookieJar -> ClientEnv
mkClientEnvWithCookies manager config cookieJar =
  let baseEnv = Servant.mkClientEnv manager (mkBaseUrl config)
   in baseEnv{Servant.cookieJar = Just cookieJar}

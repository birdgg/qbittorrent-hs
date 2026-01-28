-- | qBittorrent Web API client for Haskell
--
-- This library provides a type-safe client for the qBittorrent Web API
-- using servant-client.
--
-- = Quick Start
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Network.QBittorrent
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   cookieJar <- newCookieJar
--   let config = defaultConfig
--       env = mkClientEnvWithCookies manager config cookieJar
--
--   -- Login
--   result <- runClientM (login config) env
--   case result of
--     Right "Ok." -> putStrLn "Logged in!"
--     _ -> putStrLn "Login failed"
--
--   -- Get torrents
--   torrents <- runClientM (getTorrents Nothing) env
--   print torrents
-- @
--
-- = Session Management
--
-- qBittorrent uses session cookies for authentication. Use 'mkClientEnvWithCookies'
-- to create a 'ClientEnv' that persists cookies across requests.
module Network.QBittorrent
  ( -- * Client
    module Network.QBittorrent.Client

    -- * API Definition
  , module Network.QBittorrent.API
  ) where

import Network.QBittorrent.API
import Network.QBittorrent.Client

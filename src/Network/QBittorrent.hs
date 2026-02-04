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
-- import Network.QBittorrent qualified as QB
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   client <- QB.newClient manager QB.defaultConfig
--
--   -- Login
--   result <- QB.runQB client (QB.login QB.defaultConfig)
--   case result of
--     Right "Ok." -> putStrLn "Logged in!"
--     _ -> putStrLn "Login failed"
--
--   -- Get torrents (session cookie is managed automatically)
--   torrents <- QB.runQB client (QB.getTorrents Nothing)
--   print torrents
-- @
--
-- = Session Management
--
-- The 'QBClient' manages session cookies automatically. Create a client with
-- 'newClient' and use 'runQB' to execute requests.
module Network.QBittorrent
  ( -- * Client
    module Network.QBittorrent.Client

    -- * API Definition
  , module Network.QBittorrent.API
  ) where

import Network.QBittorrent.API
import Network.QBittorrent.Client

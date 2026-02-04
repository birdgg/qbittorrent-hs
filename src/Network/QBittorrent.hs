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
--   client <- newClient manager defaultConfig
--
--   -- Login
--   result <- runQB client (login defaultConfig)
--   case result of
--     Right "Ok." -> putStrLn "Logged in!"
--     _ -> putStrLn "Login failed"
--
--   -- Get torrents (session cookie is managed automatically)
--   torrents <- runQB client (getTorrents Nothing)
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

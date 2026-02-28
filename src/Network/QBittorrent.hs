-- | qBittorrent Web API client for Haskell
--
-- This library provides a type-safe client for the qBittorrent Web API
-- using servant-client.
--
-- = Quick Start (Simple Client)
--
-- For the simplest usage, use "Network.QBittorrent.SimpleClient" which provides
-- a record-based API with @IO (Either QBClientError a)@ return types:
--
-- @
-- import Network.QBittorrent.SimpleClient
--
-- main :: IO ()
-- main = do
--   result <- newQBittorrentClient defaultConfig
--   case result of
--     Left err -> print err
--     Right client -> do
--       torrents <- client.getTorrents Nothing
--       print torrents
-- @
--
-- = Advanced Usage (ClientM)
--
-- For more control, use the servant-client based API:
--
-- @
-- import Network.QBittorrent qualified as QB
--
-- main :: IO ()
-- main = do
--   client <- QB.initQBClient QB.defaultConfig
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
-- For custom HTTP manager settings, use 'initQBClientWith':
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Network.QBittorrent qualified as QB
--
-- manager <- newManager tlsManagerSettings
-- client <- QB.initQBClientWith manager QB.defaultConfig
-- @
--
-- = Session Management
--
-- Both clients manage session cookies automatically. The simple client handles
-- login during creation, while the advanced client requires explicit login.
module Network.QBittorrent
  ( -- * Client
    module Network.QBittorrent.Client

    -- * API Definition
  , module Network.QBittorrent.API
  ) where

import Network.QBittorrent.API
import Network.QBittorrent.Client

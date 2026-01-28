-- | qBittorrent Web API client using servant-client
--
-- This module provides ClientM functions for interacting with the qBittorrent Web API.
--
-- = Usage
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Network.QBittorrent.Client
-- import Network.QBittorrent.Client.Auth
-- import Servant.Client (runClientM)
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   cookieJar <- newCookieJar
--   let config = defaultConfig { host = "localhost", port = 8080 }
--       env = mkClientEnvWithCookies manager config cookieJar
--
--   -- Login first
--   loginResult <- runClientM (login config) env
--   case loginResult of
--     Right "Ok." -> do
--       -- Get all torrents
--       torrents <- runClientM (getTorrents Nothing) env
--       print torrents
--     _ -> putStrLn "Login failed"
-- @
module Network.QBittorrent.Client
  ( -- * Auth Operations
    login
  , logout

    -- * Torrent Operations
  , addTorrent
  , getTorrents
  , getTorrentFiles
  , pauseTorrents
  , resumeTorrents
  , deleteTorrents
  , addTags
  , removeTags
  , renameFile
  , renameFolder
  , setLocation

    -- * App Operations
  , getVersion
  , getWebapiVersion
  , getBuildInfo
  , shutdownApp
  , getPreferences
  , setPreferences
  , getDefaultSavePath

    -- * Sync Operations
  , syncMaindata
  , syncTorrentPeers

    -- * Re-exports
  , module Network.QBittorrent.Types
  , module Network.QBittorrent.Client.Auth
  , mkBaseUrl
  , ClientM
  , runClientM
  , ClientError
  ) where

import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Network.QBittorrent.API (QBittorrentRoutes (..), AuthRoutes (..), TorrentsRoutes (..), AppRoutes (..), SyncRoutes (..), mkBaseUrl)
import Network.QBittorrent.Client.Auth
import Network.QBittorrent.Types
import Servant.API (NoContent)
import Servant.Client (ClientError, ClientM, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Generated client functions record
qbClient :: QBittorrentRoutes (AsClientT ClientM)
qbClient = genericClient

-- | Login to qBittorrent
--
-- Returns "Ok." on success or "Fails." on failure.
login :: QBConfig -> ClientM Text
login cfg = qbClient.auth.login (LoginForm cfg.username cfg.password)

-- | Logout from qBittorrent
logout :: ClientM NoContent
logout = qbClient.auth.logout

-- | Add torrent by URL or magnet link
addTorrent :: AddTorrentRequest -> ClientM Text
addTorrent = qbClient.torrents.add

-- | Get torrents info
--
-- Pass 'Nothing' to get all torrents, or 'Just' a request to filter.
getTorrents :: Maybe TorrentInfoRequest -> ClientM [TorrentInfo]
getTorrents mReq = case mReq of
  Nothing -> qbClient.torrents.info Nothing Nothing Nothing Nothing
  Just req ->
    qbClient.torrents.info
      (torrentFilterToText <$> req.filter_)
      req.category
      req.tag
      req.hashes

-- | Get files within a torrent
getTorrentFiles :: Text -> ClientM [TorrentFile]
getTorrentFiles = qbClient.torrents.files

-- | Pause torrents by hash
pauseTorrents :: [Text] -> ClientM NoContent
pauseTorrents hashes = qbClient.torrents.pause (HashesForm $ T.intercalate "|" hashes)

-- | Resume torrents by hash
resumeTorrents :: [Text] -> ClientM NoContent
resumeTorrents hashes = qbClient.torrents.resume (HashesForm $ T.intercalate "|" hashes)

-- | Delete torrents
--
-- Set 'deleteFiles' to 'True' to also delete downloaded files.
deleteTorrents :: [Text] -> Bool -> ClientM NoContent
deleteTorrents hashes deleteFiles =
  qbClient.torrents.delete
    (DeleteTorrentsForm (T.intercalate "|" hashes) (if deleteFiles then "true" else "false"))

-- | Add tags to torrents
addTags :: [Text] -> [Text] -> ClientM NoContent
addTags hashes tags =
  qbClient.torrents.addTags (TagsForm (T.intercalate "|" hashes) (T.intercalate "," tags))

-- | Remove tags from torrents
removeTags :: [Text] -> [Text] -> ClientM NoContent
removeTags hashes tags =
  qbClient.torrents.removeTags (TagsForm (T.intercalate "|" hashes) (T.intercalate "," tags))

-- | Rename a file within a torrent
renameFile :: Text -> Text -> Text -> ClientM NoContent
renameFile hash oldPath newPath =
  qbClient.torrents.renameFile (RenameFileForm hash oldPath newPath)

-- | Rename a folder within a torrent
renameFolder :: Text -> Text -> Text -> ClientM NoContent
renameFolder hash oldPath newPath =
  qbClient.torrents.renameFolder (RenameFolderForm hash oldPath newPath)

-- | Set save location for torrents
setLocation :: [Text] -> Text -> ClientM NoContent
setLocation hashes location =
  qbClient.torrents.setLocation (SetLocationForm (T.intercalate "|" hashes) location)

-- | Get qBittorrent application version
--
-- Returns version string like "v4.6.2"
getVersion :: ClientM Text
getVersion = qbClient.app.version

-- | Get WebAPI version
--
-- Returns version string like "2.9.3"
getWebapiVersion :: ClientM Text
getWebapiVersion = qbClient.app.webapiVersion

-- | Get build info
--
-- Returns build information including Qt, libtorrent, Boost, and OpenSSL versions.
getBuildInfo :: ClientM BuildInfo
getBuildInfo = qbClient.app.buildInfo

-- | Shutdown qBittorrent application
--
-- WARNING: This will terminate the qBittorrent process.
shutdownApp :: ClientM NoContent
shutdownApp = qbClient.app.shutdown

-- | Get qBittorrent preferences
--
-- Returns all application preferences.
getPreferences :: ClientM Preferences
getPreferences = qbClient.app.preferences

-- | Set qBittorrent preferences
--
-- Only fields with 'Just' values will be updated.
-- Use 'defaultPreferences' as a base and set only the fields you want to change.
setPreferences :: Preferences -> ClientM NoContent
setPreferences prefs =
  qbClient.app.setPreferences (PreferencesForm jsonStr)
  where
    jsonStr = TL.toStrict $ TLE.decodeUtf8 $ Aeson.encode prefs

-- | Get default save path
getDefaultSavePath :: ClientM Text
getDefaultSavePath = qbClient.app.defaultSavePath

-- | Get sync maindata
--
-- Pass 0 for the first request to get full data.
-- Pass the returned 'rid' for subsequent requests to get incremental updates.
syncMaindata :: Int64 -> ClientM SyncMainData
syncMaindata = qbClient.sync.maindata

-- | Get torrent peers with incremental updates
--
-- Pass the torrent hash and optionally a rid from a previous response.
-- Pass 'Nothing' for rid on the first request to get full data.
syncTorrentPeers :: Text -> Maybe Int64 -> ClientM SyncTorrentPeers
syncTorrentPeers hash rid = qbClient.sync.torrentPeers hash rid

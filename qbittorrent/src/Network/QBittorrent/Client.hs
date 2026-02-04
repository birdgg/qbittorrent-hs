-- | qBittorrent Web API client using servant-client
--
-- This module provides functions for interacting with the qBittorrent Web API.
-- Import qualified as @QB@ for a clean API:
--
-- = Usage
--
-- @
-- import Network.QBittorrent.Client qualified as QB
--
-- main :: IO ()
-- main = do
--   let config = QB.defaultConfig { QB.host = "localhost", QB.port = 8080 }
--   client <- QB.newClient config
--
--   -- Login first
--   loginResult <- QB.runQB client (QB.login config)
--   case loginResult of
--     Right "Ok." -> do
--       -- Get all torrents (session cookie is managed automatically)
--       torrents <- QB.runQB client (QB.getTorrents Nothing)
--       print torrents
--     _ -> putStrLn "Login failed"
-- @
--
-- For custom HTTP manager settings, use 'newClientWith':
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
--
-- manager <- newManager tlsManagerSettings
-- client <- QB.newClientWith manager config
-- @
module Network.QBittorrent.Client
  ( -- * Record API
    -- | Use record field accessors directly: @QB.login@, @QB.getTorrents@, etc.
    login
  , logout
  , addTorrent
  , getTorrents
  , getTorrentFiles
  , stopTorrents
  , startTorrents
  , deleteTorrents
  , addTags
  , removeTags
  , renameFile
  , renameFolder
  , setLocation
  , getTorrentProperties
  , getTorrentTrackers
  , getTorrentWebSeeds
  , getTorrentPieceStates
  , getTorrentPieceHashes
  , exportTorrent
  , recheckTorrents
  , reannounceTorrents
  , increasePriority
  , decreasePriority
  , setTopPriority
  , setBottomPriority
  , setFilePriority
  , setTorrentDownloadLimit
  , setTorrentUploadLimit
  , setTorrentShareLimits
  , setSuperSeeding
  , setForceStart
  , setAutoManagement
  , toggleSequentialDownload
  , toggleFirstLastPiecePriority
  , getCategories
  , setTorrentCategory
  , createCategory
  , editCategory
  , removeCategories
  , getTags
  , createGlobalTags
  , deleteGlobalTags
  , addTorrentTrackers
  , editTorrentTracker
  , removeTorrentTrackers
  , addTorrentPeers
  , renameTorrent
  , getVersion
  , getWebapiVersion
  , getBuildInfo
  , shutdownApp
  , getPreferences
  , setPreferences
  , getDefaultSavePath
  , getNetworkInterfaces
  , getNetworkInterfaceAddresses
    -- ** Torrent Extensions
  , getTorrentsCount
  , getTorrentDownloadLimits
  , getTorrentUploadLimits
    -- ** Log API
  , getMainLog
  , getPeersLog
    -- ** Sync API
  , syncMaindata
  , syncTorrentPeers
    -- ** Transfer API
  , getTransferInfo
  , getSpeedLimitsMode
  , toggleSpeedLimitsMode
  , getGlobalDownloadLimit
  , setGlobalDownloadLimit
  , getGlobalUploadLimit
  , setGlobalUploadLimit
  , banPeers

    -- * Client
  , QBClient (..)
  , newClient
  , newClientWith
  , runQB

    -- * Re-exports
  , module Network.QBittorrent.Types
  , mkBaseUrl
  , ClientM
  , ClientError
  ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Text.Read (readMaybe)
import Network.HTTP.Client (CookieJar, Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent.API (QBittorrentRoutes (..), AuthRoutes (..), TorrentsRoutes (..), AppRoutes (..), LogRoutes (..), SyncRoutes (..), TransferRoutes (..), mkBaseUrl)
import Network.QBittorrent.Types
import Servant.Client qualified as Servant
import Servant.API (NoContent)
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- -----------------------------------------------------------------------------
-- Client
-- -----------------------------------------------------------------------------

-- | qBittorrent client with automatic session management
--
-- The client maintains a cookie jar internally for session persistence.
-- Create with 'newClient' and use with 'runQB'.
data QBClient = QBClient
  { clientEnv :: ClientEnv
  , cookieJar :: TVar CookieJar
  , config :: QBConfig
  }

-- | Create a new qBittorrent client with default TLS manager
--
-- The cookie jar and HTTP manager are created and managed internally.
-- This is the simplest way to create a client.
--
-- @
-- client <- newClient config
-- result <- runQB client (login config)
-- @
newClient :: QBConfig -> IO QBClient
newClient cfg = do
  manager <- newManager tlsManagerSettings
  newClientWith manager cfg

-- | Create a new qBittorrent client with a custom HTTP manager
--
-- Use this when you need to share a manager across multiple clients
-- or require custom manager settings.
--
-- @
-- manager <- newManager tlsManagerSettings
-- client <- newClientWith manager config
-- result <- runQB client (login config)
-- @
newClientWith :: Manager -> QBConfig -> IO QBClient
newClientWith manager cfg = do
  jar <- newTVarIO mempty
  let baseEnv = Servant.mkClientEnv manager (mkBaseUrl cfg)
      env = baseEnv{Servant.cookieJar = Just jar}
  pure QBClient{clientEnv = env, cookieJar = jar, config = cfg}

-- | Run a qBittorrent API request
--
-- The session cookie is automatically maintained across requests.
runQB :: QBClient -> ClientM a -> IO (Either ClientError a)
runQB client action = runClientM action client.clientEnv

-- | Internal generated client functions
qbClient :: QBittorrentRoutes (AsClientT ClientM)
qbClient = genericClient

-- -----------------------------------------------------------------------------
-- Auth Operations
-- -----------------------------------------------------------------------------

-- | Login to qBittorrent
--
-- Returns "Ok." on success or "Fails." on failure.
login :: QBConfig -> ClientM Text
login cfg = qbClient.auth.login (LoginForm cfg.username cfg.password)

-- | Logout from qBittorrent
logout :: ClientM NoContent
logout = qbClient.auth.logout

-- -----------------------------------------------------------------------------
-- Torrent Operations
-- -----------------------------------------------------------------------------

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

-- | Stop torrents by hash
stopTorrents :: [Text] -> ClientM NoContent
stopTorrents hashes = qbClient.torrents.stop (HashesForm $ T.intercalate "|" hashes)

-- | Start torrents by hash
startTorrents :: [Text] -> ClientM NoContent
startTorrents hashes = qbClient.torrents.start (HashesForm $ T.intercalate "|" hashes)

-- | Delete torrents
--
-- Set 'deleteFiles' to 'True' to also delete downloaded files.
deleteTorrents :: [Text] -> Bool -> ClientM NoContent
deleteTorrents hashes deleteFiles_ =
  qbClient.torrents.delete
    (DeleteTorrentsForm (T.intercalate "|" hashes) (if deleteFiles_ then "true" else "false"))

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

-- -----------------------------------------------------------------------------
-- Torrent Query Operations
-- -----------------------------------------------------------------------------

-- | Get general properties of a torrent
getTorrentProperties :: Text -> ClientM TorrentProperties
getTorrentProperties = qbClient.torrents.properties

-- | Get trackers for a torrent
getTorrentTrackers :: Text -> ClientM [TorrentTracker]
getTorrentTrackers = qbClient.torrents.trackers

-- | Get web seeds for a torrent
getTorrentWebSeeds :: Text -> ClientM [TorrentWebSeed]
getTorrentWebSeeds = qbClient.torrents.webseeds

-- | Get piece states for a torrent
--
-- Returns a list of integers where:
-- 0 = not downloaded, 1 = downloading, 2 = downloaded
getTorrentPieceStates :: Text -> ClientM [Int]
getTorrentPieceStates = qbClient.torrents.pieceStates

-- | Get piece hashes for a torrent
getTorrentPieceHashes :: Text -> ClientM [Text]
getTorrentPieceHashes = qbClient.torrents.pieceHashes

-- | Export a torrent as .torrent file
exportTorrent :: Text -> ClientM ByteString
exportTorrent = qbClient.torrents.export

-- -----------------------------------------------------------------------------
-- Priority Management
-- -----------------------------------------------------------------------------

-- | Recheck torrents
recheckTorrents :: [Text] -> ClientM NoContent
recheckTorrents hashes = qbClient.torrents.recheck (HashesForm $ T.intercalate "|" hashes)

-- | Reannounce torrents to trackers
reannounceTorrents :: [Text] -> ClientM NoContent
reannounceTorrents hashes = qbClient.torrents.reannounce (HashesForm $ T.intercalate "|" hashes)

-- | Increase priority of torrents
increasePriority :: [Text] -> ClientM NoContent
increasePriority hashes = qbClient.torrents.increasePrio (HashesForm $ T.intercalate "|" hashes)

-- | Decrease priority of torrents
decreasePriority :: [Text] -> ClientM NoContent
decreasePriority hashes = qbClient.torrents.decreasePrio (HashesForm $ T.intercalate "|" hashes)

-- | Set torrents to maximum priority
setTopPriority :: [Text] -> ClientM NoContent
setTopPriority hashes = qbClient.torrents.topPrio (HashesForm $ T.intercalate "|" hashes)

-- | Set torrents to minimum priority
setBottomPriority :: [Text] -> ClientM NoContent
setBottomPriority hashes = qbClient.torrents.bottomPrio (HashesForm $ T.intercalate "|" hashes)

-- -----------------------------------------------------------------------------
-- Limit Settings
-- -----------------------------------------------------------------------------

-- | Set file priority within a torrent
--
-- Priority values: 0 = do not download, 1 = normal, 6 = high, 7 = maximal
setFilePriority :: Text -> [Int] -> Int -> ClientM NoContent
setFilePriority hash fileIds priority =
  qbClient.torrents.setFilePrio (FilePrioForm hash (T.intercalate "|" $ map (T.pack . show) fileIds) priority)

-- | Set download speed limit for torrents (bytes/second, -1 for unlimited)
setTorrentDownloadLimit :: [Text] -> Int -> ClientM NoContent
setTorrentDownloadLimit hashes limit =
  qbClient.torrents.setDownloadLimit (LimitForm (T.intercalate "|" hashes) limit)

-- | Set upload speed limit for torrents (bytes/second, -1 for unlimited)
setTorrentUploadLimit :: [Text] -> Int -> ClientM NoContent
setTorrentUploadLimit hashes limit =
  qbClient.torrents.setUploadLimit (LimitForm (T.intercalate "|" hashes) limit)

-- | Set share limits for torrents
--
-- ratioLimit: -2 = use global, -1 = unlimited, >= 0 = specific ratio
-- seedingTimeLimit: -2 = use global, -1 = unlimited, >= 0 = minutes
-- inactiveSeedingTimeLimit: -2 = use global, -1 = unlimited, >= 0 = minutes (v5.0+)
setTorrentShareLimits :: [Text] -> Double -> Int -> Int -> ClientM NoContent
setTorrentShareLimits hashes ratioLimit seedingTimeLimit inactiveSeedingTimeLimit =
  qbClient.torrents.setShareLimits (ShareLimitsForm (T.intercalate "|" hashes) ratioLimit seedingTimeLimit inactiveSeedingTimeLimit)

-- -----------------------------------------------------------------------------
-- Behavior Settings
-- -----------------------------------------------------------------------------

-- | Enable/disable super seeding mode for torrents
setSuperSeeding :: [Text] -> Bool -> ClientM NoContent
setSuperSeeding hashes enabled =
  qbClient.torrents.setSuperSeeding (BoolForm (T.intercalate "|" hashes) (if enabled then "true" else "false"))

-- | Enable/disable force start for torrents
setForceStart :: [Text] -> Bool -> ClientM NoContent
setForceStart hashes enabled =
  qbClient.torrents.setForceStart (BoolForm (T.intercalate "|" hashes) (if enabled then "true" else "false"))

-- | Enable/disable automatic torrent management for torrents
setAutoManagement :: [Text] -> Bool -> ClientM NoContent
setAutoManagement hashes enabled =
  qbClient.torrents.setAutoManagement (BoolForm (T.intercalate "|" hashes) (if enabled then "true" else "false"))

-- | Toggle sequential download mode for torrents
toggleSequentialDownload :: [Text] -> ClientM NoContent
toggleSequentialDownload hashes = qbClient.torrents.toggleSequentialDownload (HashesForm $ T.intercalate "|" hashes)

-- | Toggle first/last piece priority for torrents
toggleFirstLastPiecePriority :: [Text] -> ClientM NoContent
toggleFirstLastPiecePriority hashes = qbClient.torrents.toggleFirstLastPiecePrio (HashesForm $ T.intercalate "|" hashes)

-- -----------------------------------------------------------------------------
-- Category Management
-- -----------------------------------------------------------------------------

-- | Get all categories
getCategories :: ClientM (Map Text Category)
getCategories = qbClient.torrents.categories

-- | Set category for torrents
setTorrentCategory :: [Text] -> Text -> ClientM NoContent
setTorrentCategory hashes cat =
  qbClient.torrents.setCategory (CategoryForm (T.intercalate "|" hashes) cat)

-- | Create a new category
createCategory :: Text -> Text -> ClientM NoContent
createCategory cat savePath =
  qbClient.torrents.createCategory (CreateCategoryForm cat savePath)

-- | Edit an existing category
editCategory :: Text -> Text -> ClientM NoContent
editCategory cat savePath =
  qbClient.torrents.editCategory (CreateCategoryForm cat savePath)

-- | Remove categories
removeCategories :: [Text] -> ClientM NoContent
removeCategories cats =
  qbClient.torrents.removeCategories (CategoriesForm $ T.intercalate "\n" cats)

-- -----------------------------------------------------------------------------
-- Tag Management
-- -----------------------------------------------------------------------------

-- | Get all tags
getTags :: ClientM [Text]
getTags = qbClient.torrents.tags

-- | Create new global tags
createGlobalTags :: [Text] -> ClientM NoContent
createGlobalTags tagsList =
  qbClient.torrents.createTags (TagsOnlyForm $ T.intercalate "," tagsList)

-- | Delete global tags
deleteGlobalTags :: [Text] -> ClientM NoContent
deleteGlobalTags tagsList =
  qbClient.torrents.deleteTags (TagsOnlyForm $ T.intercalate "," tagsList)

-- -----------------------------------------------------------------------------
-- Tracker Management
-- -----------------------------------------------------------------------------

-- | Add trackers to a torrent
addTorrentTrackers :: Text -> [Text] -> ClientM NoContent
addTorrentTrackers hash urls =
  qbClient.torrents.addTrackers (AddTrackersForm hash (T.intercalate "\n" urls))

-- | Edit a tracker URL for a torrent
editTorrentTracker :: Text -> Text -> Text -> ClientM NoContent
editTorrentTracker hash origUrl newUrl =
  qbClient.torrents.editTracker (EditTrackerForm hash origUrl newUrl)

-- | Remove trackers from a torrent
removeTorrentTrackers :: Text -> [Text] -> ClientM NoContent
removeTorrentTrackers hash urls =
  qbClient.torrents.removeTrackers (RemoveTrackersForm hash (T.intercalate "|" urls))

-- | Add peers to torrents
addTorrentPeers :: [Text] -> [Text] -> ClientM NoContent
addTorrentPeers hashes peers =
  qbClient.torrents.addPeers (AddPeersForm (T.intercalate "|" hashes) (T.intercalate "|" peers))

-- -----------------------------------------------------------------------------
-- Rename Operations
-- -----------------------------------------------------------------------------

-- | Rename a torrent
renameTorrent :: Text -> Text -> ClientM NoContent
renameTorrent hash newName =
  qbClient.torrents.rename (RenameForm hash newName)

-- -----------------------------------------------------------------------------
-- App Operations
-- -----------------------------------------------------------------------------

-- | Get qBittorrent application version
--
-- Returns version string like "v5.0.0"
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

-- | Get list of network interfaces
getNetworkInterfaces :: ClientM [Text]
getNetworkInterfaces = qbClient.app.networkInterfaceList

-- | Get addresses for a network interface
getNetworkInterfaceAddresses :: Text -> ClientM [Text]
getNetworkInterfaceAddresses = qbClient.app.networkInterfaceAddressList

-- -----------------------------------------------------------------------------
-- Torrent Extensions
-- -----------------------------------------------------------------------------

-- | Get total number of torrents
getTorrentsCount :: ClientM Int
getTorrentsCount = qbClient.torrents.count

-- | Get download limits for torrents
--
-- Returns a map of hash -> limit (bytes/s, -1 for unlimited)
getTorrentDownloadLimits :: [Text] -> ClientM (Map Text Int)
getTorrentDownloadLimits hashes =
  qbClient.torrents.downloadLimit (T.intercalate "|" hashes)

-- | Get upload limits for torrents
--
-- Returns a map of hash -> limit (bytes/s, -1 for unlimited)
getTorrentUploadLimits :: [Text] -> ClientM (Map Text Int)
getTorrentUploadLimits hashes =
  qbClient.torrents.uploadLimit (T.intercalate "|" hashes)

-- -----------------------------------------------------------------------------
-- Log Operations
-- -----------------------------------------------------------------------------

-- | Get main log entries
--
-- Pass flags to filter log types. Pass 'Nothing' for lastKnownId to get all logs,
-- or 'Just id' to get only logs newer than that id.
getMainLog
  :: Bool       -- ^ Include normal messages
  -> Bool       -- ^ Include info messages
  -> Bool       -- ^ Include warning messages
  -> Bool       -- ^ Include critical messages
  -> Maybe Int64  -- ^ Last known id (exclude older entries)
  -> ClientM [LogEntry]
getMainLog normal info warning critical lastKnownId =
  qbClient.log.main normal info warning critical lastKnownId

-- | Get peer log entries
--
-- Pass 'Nothing' for lastKnownId to get all logs,
-- or 'Just id' to get only logs newer than that id.
getPeersLog :: Maybe Int64 -> ClientM [PeerLogEntry]
getPeersLog = qbClient.log.peers

-- -----------------------------------------------------------------------------
-- Sync Operations
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- Transfer Operations
-- -----------------------------------------------------------------------------

-- | Get global transfer info
--
-- Returns current transfer statistics including speeds and totals.
getTransferInfo :: ClientM TransferInfo
getTransferInfo = qbClient.transfer.info

-- | Get alternative speed limits mode
--
-- Returns True if alternative speed limits are enabled, False otherwise.
getSpeedLimitsMode :: ClientM Bool
getSpeedLimitsMode = do
  result <- qbClient.transfer.speedLimitsMode
  pure $ T.strip result == "1"

-- | Toggle alternative speed limits on/off
toggleSpeedLimitsMode :: ClientM NoContent
toggleSpeedLimitsMode = qbClient.transfer.toggleSpeedLimitsMode

-- | Get global download limit
--
-- Returns the limit in bytes/s, or 0 for unlimited.
getGlobalDownloadLimit :: ClientM Int
getGlobalDownloadLimit = do
  result <- qbClient.transfer.downloadLimit
  pure $ fromMaybe 0 $ readMaybe $ T.unpack $ T.strip result

-- | Set global download limit
--
-- Pass 0 for unlimited.
setGlobalDownloadLimit :: Int -> ClientM NoContent
setGlobalDownloadLimit limit =
  qbClient.transfer.setDownloadLimit (TransferLimitForm limit)

-- | Get global upload limit
--
-- Returns the limit in bytes/s, or 0 for unlimited.
getGlobalUploadLimit :: ClientM Int
getGlobalUploadLimit = do
  result <- qbClient.transfer.uploadLimit
  pure $ fromMaybe 0 $ readMaybe $ T.unpack $ T.strip result

-- | Set global upload limit
--
-- Pass 0 for unlimited.
setGlobalUploadLimit :: Int -> ClientM NoContent
setGlobalUploadLimit limit =
  qbClient.transfer.setUploadLimit (TransferLimitForm limit)

-- | Ban peers permanently
--
-- Each peer should be in format "host:port"
banPeers :: [Text] -> ClientM NoContent
banPeers peers =
  qbClient.transfer.banPeers (BanPeersForm $ T.intercalate "\n" peers)

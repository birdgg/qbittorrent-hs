-- | Effectful bindings for qBittorrent Web API
--
-- This module provides a static effect for the qBittorrent Web API,
-- allowing seamless integration with the effectful ecosystem.
--
-- = Usage
--
-- @
-- import Effectful
-- import Effectful.QBittorrent
--
-- main :: IO ()
-- main = do
--   client <- newClient defaultConfig
--   result <- runEff . runQBittorrent client $ do
--     loginResult <- login defaultConfig
--     case loginResult of
--       Right "Ok." -> getTorrents Nothing
--       _ -> pure (Left $ AuthError "Login failed")
--   print result
-- @
module Effectful.QBittorrent
  ( -- * Effect
    QBittorrent
  , runQBittorrent

    -- * Low-level Operations
  , getQBClient
  , liftQB

    -- * Auth Operations
  , login
  , logout

    -- * Torrent Operations
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

    -- * Torrent Query Operations
  , getTorrentProperties
  , getTorrentTrackers
  , getTorrentWebSeeds
  , getTorrentPieceStates
  , getTorrentPieceHashes
  , exportTorrent

    -- * Priority Management
  , recheckTorrents
  , reannounceTorrents
  , increasePriority
  , decreasePriority
  , setTopPriority
  , setBottomPriority

    -- * Limit Settings
  , setFilePriority
  , setTorrentDownloadLimit
  , setTorrentUploadLimit
  , setTorrentShareLimits

    -- * Behavior Settings
  , setSuperSeeding
  , setForceStart
  , setAutoManagement
  , toggleSequentialDownload
  , toggleFirstLastPiecePriority

    -- * Category Management
  , getCategories
  , setTorrentCategory
  , createCategory
  , editCategory
  , removeCategories

    -- * Tag Management
  , getTags
  , createGlobalTags
  , deleteGlobalTags

    -- * Tracker Management
  , addTorrentTrackers
  , editTorrentTracker
  , removeTorrentTrackers
  , addTorrentPeers

    -- * Rename Operations
  , renameTorrent

    -- * App Operations
  , getVersion
  , getWebapiVersion
  , getBuildInfo
  , shutdownApp
  , getPreferences
  , setPreferences
  , getDefaultSavePath
  , getNetworkInterfaces
  , getNetworkInterfaceAddresses

    -- * Torrent Extensions
  , getTorrentsCount
  , getTorrentDownloadLimits
  , getTorrentUploadLimits

    -- * Log Operations
  , getMainLog
  , getPeersLog

    -- * Sync Operations
  , syncMaindata
  , syncTorrentPeers

    -- * Transfer Operations
  , getTransferInfo
  , getSpeedLimitsMode
  , toggleSpeedLimitsMode
  , getGlobalDownloadLimit
  , setGlobalDownloadLimit
  , getGlobalUploadLimit
  , setGlobalUploadLimit
  , banPeers

    -- * Re-exports
  , module Network.QBittorrent.Types
  , QBClient
  , newClient
  , newClientWith
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Bifunctor (first)
import Effectful
import Effectful.Dispatch.Static
import Network.QBittorrent.Client (QBClient, newClient, newClientWith)
import qualified Network.QBittorrent.Client as QB
import Network.QBittorrent.Types
import Servant.API (NoContent)

-- | qBittorrent effect for interacting with qBittorrent Web API
data QBittorrent :: Effect

type instance DispatchOf QBittorrent = Static WithSideEffects

newtype instance StaticRep QBittorrent = QBittorrentRep QBClient

-- | Run qBittorrent operations with a client
--
-- @
-- result <- runEff . runQBittorrent client $ do
--   login config
--   getTorrents Nothing
-- @
runQBittorrent
  :: IOE :> es
  => QBClient
  -> Eff (QBittorrent : es) a
  -> Eff es a
runQBittorrent client = evalStaticRep (QBittorrentRep client)

-- | Get the underlying QBClient
--
-- Useful for advanced operations or debugging.
getQBClient :: QBittorrent :> es => Eff es QBClient
getQBClient = do
  QBittorrentRep client <- getStaticRep
  pure client

-- | Lift a ClientM action into the QBittorrent effect
--
-- Use this for operations not yet wrapped by this module.
liftQB
  :: (QBittorrent :> es, IOE :> es)
  => QB.ClientM a
  -> Eff es (Either QBError a)
liftQB action = do
  client <- getQBClient
  result <- unsafeEff_ $ QB.runQB client action
  pure $ first clientErrorToQBError result

-- -----------------------------------------------------------------------------
-- Auth Operations
-- -----------------------------------------------------------------------------

-- | Login to qBittorrent
--
-- Returns "Ok." on success or "Fails." on failure.
login
  :: (QBittorrent :> es, IOE :> es)
  => QBConfig
  -> Eff es (Either QBError Text)
login cfg = liftQB (QB.login cfg)

-- | Logout from qBittorrent
logout
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError NoContent)
logout = liftQB QB.logout

-- -----------------------------------------------------------------------------
-- Torrent Operations
-- -----------------------------------------------------------------------------

-- | Add torrent by URL or magnet link
addTorrent
  :: (QBittorrent :> es, IOE :> es)
  => AddTorrentRequest
  -> Eff es (Either QBError Text)
addTorrent req = liftQB (QB.addTorrent req)

-- | Get torrents info
--
-- Pass 'Nothing' to get all torrents, or 'Just' a request to filter.
getTorrents
  :: (QBittorrent :> es, IOE :> es)
  => Maybe TorrentInfoRequest
  -> Eff es (Either QBError [TorrentInfo])
getTorrents mReq = liftQB (QB.getTorrents mReq)

-- | Get files within a torrent
getTorrentFiles
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError [TorrentFile])
getTorrentFiles hash = liftQB (QB.getTorrentFiles hash)

-- | Stop torrents by hash
stopTorrents
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
stopTorrents hashes = liftQB (QB.stopTorrents hashes)

-- | Start torrents by hash
startTorrents
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
startTorrents hashes = liftQB (QB.startTorrents hashes)

-- | Delete torrents
--
-- Set 'deleteFiles' to 'True' to also delete downloaded files.
deleteTorrents
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Bool
  -> Eff es (Either QBError NoContent)
deleteTorrents hashes deleteFiles_ = liftQB (QB.deleteTorrents hashes deleteFiles_)

-- | Add tags to torrents
addTags
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> [Text]
  -> Eff es (Either QBError NoContent)
addTags hashes tags = liftQB (QB.addTags hashes tags)

-- | Remove tags from torrents
removeTags
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> [Text]
  -> Eff es (Either QBError NoContent)
removeTags hashes tags = liftQB (QB.removeTags hashes tags)

-- | Rename a file within a torrent
renameFile
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Text
  -> Text
  -> Eff es (Either QBError NoContent)
renameFile hash oldPath newPath = liftQB (QB.renameFile hash oldPath newPath)

-- | Rename a folder within a torrent
renameFolder
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Text
  -> Text
  -> Eff es (Either QBError NoContent)
renameFolder hash oldPath newPath = liftQB (QB.renameFolder hash oldPath newPath)

-- | Set save location for torrents
setLocation
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Text
  -> Eff es (Either QBError NoContent)
setLocation hashes location = liftQB (QB.setLocation hashes location)

-- -----------------------------------------------------------------------------
-- Torrent Query Operations
-- -----------------------------------------------------------------------------

-- | Get general properties of a torrent
getTorrentProperties
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError TorrentProperties)
getTorrentProperties hash = liftQB (QB.getTorrentProperties hash)

-- | Get trackers for a torrent
getTorrentTrackers
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError [TorrentTracker])
getTorrentTrackers hash = liftQB (QB.getTorrentTrackers hash)

-- | Get web seeds for a torrent
getTorrentWebSeeds
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError [TorrentWebSeed])
getTorrentWebSeeds hash = liftQB (QB.getTorrentWebSeeds hash)

-- | Get piece states for a torrent
--
-- Returns a list of integers where:
-- 0 = not downloaded, 1 = downloading, 2 = downloaded
getTorrentPieceStates
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError [Int])
getTorrentPieceStates hash = liftQB (QB.getTorrentPieceStates hash)

-- | Get piece hashes for a torrent
getTorrentPieceHashes
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError [Text])
getTorrentPieceHashes hash = liftQB (QB.getTorrentPieceHashes hash)

-- | Export a torrent as .torrent file
exportTorrent
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError ByteString)
exportTorrent hash = liftQB (QB.exportTorrent hash)

-- -----------------------------------------------------------------------------
-- Priority Management
-- -----------------------------------------------------------------------------

-- | Recheck torrents
recheckTorrents
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
recheckTorrents hashes = liftQB (QB.recheckTorrents hashes)

-- | Reannounce torrents to trackers
reannounceTorrents
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
reannounceTorrents hashes = liftQB (QB.reannounceTorrents hashes)

-- | Increase priority of torrents
increasePriority
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
increasePriority hashes = liftQB (QB.increasePriority hashes)

-- | Decrease priority of torrents
decreasePriority
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
decreasePriority hashes = liftQB (QB.decreasePriority hashes)

-- | Set torrents to maximum priority
setTopPriority
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
setTopPriority hashes = liftQB (QB.setTopPriority hashes)

-- | Set torrents to minimum priority
setBottomPriority
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
setBottomPriority hashes = liftQB (QB.setBottomPriority hashes)

-- -----------------------------------------------------------------------------
-- Limit Settings
-- -----------------------------------------------------------------------------

-- | Set file priority within a torrent
--
-- Priority values: 0 = do not download, 1 = normal, 6 = high, 7 = maximal
setFilePriority
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> [Int]
  -> Int
  -> Eff es (Either QBError NoContent)
setFilePriority hash fileIds priority = liftQB (QB.setFilePriority hash fileIds priority)

-- | Set download speed limit for torrents (bytes/second, -1 for unlimited)
setTorrentDownloadLimit
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Int
  -> Eff es (Either QBError NoContent)
setTorrentDownloadLimit hashes limit = liftQB (QB.setTorrentDownloadLimit hashes limit)

-- | Set upload speed limit for torrents (bytes/second, -1 for unlimited)
setTorrentUploadLimit
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Int
  -> Eff es (Either QBError NoContent)
setTorrentUploadLimit hashes limit = liftQB (QB.setTorrentUploadLimit hashes limit)

-- | Set share limits for torrents
--
-- ratioLimit: -2 = use global, -1 = unlimited, >= 0 = specific ratio
-- seedingTimeLimit: -2 = use global, -1 = unlimited, >= 0 = minutes
-- inactiveSeedingTimeLimit: -2 = use global, -1 = unlimited, >= 0 = minutes (v5.0+)
setTorrentShareLimits
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Double
  -> Int
  -> Int
  -> Eff es (Either QBError NoContent)
setTorrentShareLimits hashes ratioLimit seedingTimeLimit inactiveSeedingTimeLimit =
  liftQB (QB.setTorrentShareLimits hashes ratioLimit seedingTimeLimit inactiveSeedingTimeLimit)

-- -----------------------------------------------------------------------------
-- Behavior Settings
-- -----------------------------------------------------------------------------

-- | Enable/disable super seeding mode for torrents
setSuperSeeding
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Bool
  -> Eff es (Either QBError NoContent)
setSuperSeeding hashes enabled = liftQB (QB.setSuperSeeding hashes enabled)

-- | Enable/disable force start for torrents
setForceStart
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Bool
  -> Eff es (Either QBError NoContent)
setForceStart hashes enabled = liftQB (QB.setForceStart hashes enabled)

-- | Enable/disable automatic torrent management for torrents
setAutoManagement
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Bool
  -> Eff es (Either QBError NoContent)
setAutoManagement hashes enabled = liftQB (QB.setAutoManagement hashes enabled)

-- | Toggle sequential download mode for torrents
toggleSequentialDownload
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
toggleSequentialDownload hashes = liftQB (QB.toggleSequentialDownload hashes)

-- | Toggle first/last piece priority for torrents
toggleFirstLastPiecePriority
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
toggleFirstLastPiecePriority hashes = liftQB (QB.toggleFirstLastPiecePriority hashes)

-- -----------------------------------------------------------------------------
-- Category Management
-- -----------------------------------------------------------------------------

-- | Get all categories
getCategories
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError (Map Text Category))
getCategories = liftQB QB.getCategories

-- | Set category for torrents
setTorrentCategory
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Text
  -> Eff es (Either QBError NoContent)
setTorrentCategory hashes cat = liftQB (QB.setTorrentCategory hashes cat)

-- | Create a new category
createCategory
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Text
  -> Eff es (Either QBError NoContent)
createCategory cat savePath = liftQB (QB.createCategory cat savePath)

-- | Edit an existing category
editCategory
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Text
  -> Eff es (Either QBError NoContent)
editCategory cat savePath = liftQB (QB.editCategory cat savePath)

-- | Remove categories
removeCategories
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
removeCategories cats = liftQB (QB.removeCategories cats)

-- -----------------------------------------------------------------------------
-- Tag Management
-- -----------------------------------------------------------------------------

-- | Get all tags
getTags
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError [Text])
getTags = liftQB QB.getTags

-- | Create new global tags
createGlobalTags
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
createGlobalTags tagsList = liftQB (QB.createGlobalTags tagsList)

-- | Delete global tags
deleteGlobalTags
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
deleteGlobalTags tagsList = liftQB (QB.deleteGlobalTags tagsList)

-- -----------------------------------------------------------------------------
-- Tracker Management
-- -----------------------------------------------------------------------------

-- | Add trackers to a torrent
addTorrentTrackers
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> [Text]
  -> Eff es (Either QBError NoContent)
addTorrentTrackers hash urls = liftQB (QB.addTorrentTrackers hash urls)

-- | Edit a tracker URL for a torrent
editTorrentTracker
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Text
  -> Text
  -> Eff es (Either QBError NoContent)
editTorrentTracker hash origUrl newUrl = liftQB (QB.editTorrentTracker hash origUrl newUrl)

-- | Remove trackers from a torrent
removeTorrentTrackers
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> [Text]
  -> Eff es (Either QBError NoContent)
removeTorrentTrackers hash urls = liftQB (QB.removeTorrentTrackers hash urls)

-- | Add peers to torrents
addTorrentPeers
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> [Text]
  -> Eff es (Either QBError NoContent)
addTorrentPeers hashes peers = liftQB (QB.addTorrentPeers hashes peers)

-- -----------------------------------------------------------------------------
-- Rename Operations
-- -----------------------------------------------------------------------------

-- | Rename a torrent
renameTorrent
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Text
  -> Eff es (Either QBError NoContent)
renameTorrent hash newName = liftQB (QB.renameTorrent hash newName)

-- -----------------------------------------------------------------------------
-- App Operations
-- -----------------------------------------------------------------------------

-- | Get qBittorrent application version
--
-- Returns version string like "v5.0.0"
getVersion
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Text)
getVersion = liftQB QB.getVersion

-- | Get WebAPI version
--
-- Returns version string like "2.9.3"
getWebapiVersion
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Text)
getWebapiVersion = liftQB QB.getWebapiVersion

-- | Get build info
--
-- Returns build information including Qt, libtorrent, Boost, and OpenSSL versions.
getBuildInfo
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError BuildInfo)
getBuildInfo = liftQB QB.getBuildInfo

-- | Shutdown qBittorrent application
--
-- WARNING: This will terminate the qBittorrent process.
shutdownApp
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError NoContent)
shutdownApp = liftQB QB.shutdownApp

-- | Get qBittorrent preferences
--
-- Returns all application preferences.
getPreferences
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Preferences)
getPreferences = liftQB QB.getPreferences

-- | Set qBittorrent preferences
--
-- Only fields with 'Just' values will be updated.
setPreferences
  :: (QBittorrent :> es, IOE :> es)
  => Preferences
  -> Eff es (Either QBError NoContent)
setPreferences prefs = liftQB (QB.setPreferences prefs)

-- | Get default save path
getDefaultSavePath
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Text)
getDefaultSavePath = liftQB QB.getDefaultSavePath

-- | Get list of network interfaces
getNetworkInterfaces
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError [Text])
getNetworkInterfaces = liftQB QB.getNetworkInterfaces

-- | Get addresses for a network interface
getNetworkInterfaceAddresses
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Eff es (Either QBError [Text])
getNetworkInterfaceAddresses iface = liftQB (QB.getNetworkInterfaceAddresses iface)

-- -----------------------------------------------------------------------------
-- Torrent Extensions
-- -----------------------------------------------------------------------------

-- | Get total number of torrents
getTorrentsCount
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Int)
getTorrentsCount = liftQB QB.getTorrentsCount

-- | Get download limits for torrents
--
-- Returns a map of hash -> limit (bytes/s, -1 for unlimited)
getTorrentDownloadLimits
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError (Map Text Int))
getTorrentDownloadLimits hashes = liftQB (QB.getTorrentDownloadLimits hashes)

-- | Get upload limits for torrents
--
-- Returns a map of hash -> limit (bytes/s, -1 for unlimited)
getTorrentUploadLimits
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError (Map Text Int))
getTorrentUploadLimits hashes = liftQB (QB.getTorrentUploadLimits hashes)

-- -----------------------------------------------------------------------------
-- Log Operations
-- -----------------------------------------------------------------------------

-- | Get main log entries
--
-- Pass flags to filter log types. Pass 'Nothing' for lastKnownId to get all logs,
-- or 'Just id' to get only logs newer than that id.
getMainLog
  :: (QBittorrent :> es, IOE :> es)
  => Bool       -- ^ Include normal messages
  -> Bool       -- ^ Include info messages
  -> Bool       -- ^ Include warning messages
  -> Bool       -- ^ Include critical messages
  -> Maybe Int64  -- ^ Last known id (exclude older entries)
  -> Eff es (Either QBError [LogEntry])
getMainLog normal info warning critical lastKnownId =
  liftQB (QB.getMainLog normal info warning critical lastKnownId)

-- | Get peer log entries
--
-- Pass 'Nothing' for lastKnownId to get all logs,
-- or 'Just id' to get only logs newer than that id.
getPeersLog
  :: (QBittorrent :> es, IOE :> es)
  => Maybe Int64
  -> Eff es (Either QBError [PeerLogEntry])
getPeersLog lastKnownId = liftQB (QB.getPeersLog lastKnownId)

-- -----------------------------------------------------------------------------
-- Sync Operations
-- -----------------------------------------------------------------------------

-- | Get sync maindata
--
-- Pass 0 for the first request to get full data.
-- Pass the returned 'rid' for subsequent requests to get incremental updates.
syncMaindata
  :: (QBittorrent :> es, IOE :> es)
  => Int64
  -> Eff es (Either QBError SyncMainData)
syncMaindata rid = liftQB (QB.syncMaindata rid)

-- | Get torrent peers with incremental updates
--
-- Pass the torrent hash and optionally a rid from a previous response.
-- Pass 'Nothing' for rid on the first request to get full data.
syncTorrentPeers
  :: (QBittorrent :> es, IOE :> es)
  => Text
  -> Maybe Int64
  -> Eff es (Either QBError SyncTorrentPeers)
syncTorrentPeers hash rid = liftQB (QB.syncTorrentPeers hash rid)

-- -----------------------------------------------------------------------------
-- Transfer Operations
-- -----------------------------------------------------------------------------

-- | Get global transfer info
--
-- Returns current transfer statistics including speeds and totals.
getTransferInfo
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError TransferInfo)
getTransferInfo = liftQB QB.getTransferInfo

-- | Get alternative speed limits mode
--
-- Returns True if alternative speed limits are enabled, False otherwise.
getSpeedLimitsMode
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Bool)
getSpeedLimitsMode = liftQB QB.getSpeedLimitsMode

-- | Toggle alternative speed limits on/off
toggleSpeedLimitsMode
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError NoContent)
toggleSpeedLimitsMode = liftQB QB.toggleSpeedLimitsMode

-- | Get global download limit
--
-- Returns the limit in bytes/s, or 0 for unlimited.
getGlobalDownloadLimit
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Int)
getGlobalDownloadLimit = liftQB QB.getGlobalDownloadLimit

-- | Set global download limit
--
-- Pass 0 for unlimited.
setGlobalDownloadLimit
  :: (QBittorrent :> es, IOE :> es)
  => Int
  -> Eff es (Either QBError NoContent)
setGlobalDownloadLimit limit = liftQB (QB.setGlobalDownloadLimit limit)

-- | Get global upload limit
--
-- Returns the limit in bytes/s, or 0 for unlimited.
getGlobalUploadLimit
  :: (QBittorrent :> es, IOE :> es)
  => Eff es (Either QBError Int)
getGlobalUploadLimit = liftQB QB.getGlobalUploadLimit

-- | Set global upload limit
--
-- Pass 0 for unlimited.
setGlobalUploadLimit
  :: (QBittorrent :> es, IOE :> es)
  => Int
  -> Eff es (Either QBError NoContent)
setGlobalUploadLimit limit = liftQB (QB.setGlobalUploadLimit limit)

-- | Ban peers permanently
--
-- Each peer should be in format "host:port"
banPeers
  :: (QBittorrent :> es, IOE :> es)
  => [Text]
  -> Eff es (Either QBError NoContent)
banPeers peers = liftQB (QB.banPeers peers)

-- | Simplified record-based qBittorrent client
--
-- This module provides a record-style API where each operation is a simple
-- @IO (Either QBClientError a)@ function. This is more convenient for direct usage
-- compared to the servant-client @ClientM@ monad.
--
-- = Usage
--
-- @
-- import Network.QBittorrent.SimpleClient qualified as QB
--
-- main :: IO ()
-- main = do
--   -- Create client (handles login automatically)
--   Right client <- QB.initQBClient defaultConfig
--
--   -- Use record fields directly
--   torrents <- client.getTorrents Nothing
--   case torrents of
--     Right ts -> mapM_ print ts
--     Left err -> putStrLn $ "Error: " <> show err
--
--   -- Get version
--   version <- client.getVersion
--   print version
-- @
module Network.QBittorrent.SimpleClient
  ( -- * Client Record
    Client (..)

    -- * Client Creation
  , initQBClient
  , initQBClientWith

    -- * Re-exports
  , module Network.QBittorrent.Types
  ) where

import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.QBittorrent.Client qualified as QB
import Network.QBittorrent.Types
import Servant.API (NoContent)

-- | Simplified qBittorrent client with record-based API
--
-- Each field is an IO action that returns @Either QBClientError a@.
-- The client manages session cookies automatically.
data Client = Client
  { -- | Get qBittorrent application version
    getVersion :: IO (Either QBClientError Text)
  , -- | Get WebAPI version
    getWebapiVersion :: IO (Either QBClientError Text)
  , -- | Get build info
    getBuildInfo :: IO (Either QBClientError BuildInfo)
  , -- | Shutdown qBittorrent application
    shutdownApp :: IO (Either QBClientError ())
  , -- | Get qBittorrent preferences
    getPreferences :: IO (Either QBClientError Preferences)
  , -- | Set qBittorrent preferences
    setPreferences :: Preferences -> IO (Either QBClientError ())
  , -- | Get default save path
    getDefaultSavePath :: IO (Either QBClientError Text)
  , -- | Get list of network interfaces
    getNetworkInterfaces :: IO (Either QBClientError [Text])
  , -- | Get addresses for a network interface
    getNetworkInterfaceAddresses :: Text -> IO (Either QBClientError [Text])

    -- ** Torrent Operations
  , -- | Add torrent by URL or magnet link
    addTorrent :: AddTorrentRequest -> IO (Either QBClientError Text)
  , -- | Get torrents info (pass Nothing to get all)
    getTorrents :: Maybe TorrentInfoRequest -> IO (Either QBClientError [TorrentInfo])
  , -- | Get files within a torrent
    getTorrentContents :: InfoHash -> IO (Either QBClientError [TorrentContent])
  , -- | Stop torrents by hash
    stopTorrents :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Start torrents by hash
    startTorrents :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Delete torrents (second param: delete files)
    deleteTorrents :: [InfoHash] -> Bool -> IO (Either QBClientError ())
  , -- | Get total number of torrents
    getTorrentsCount :: IO (Either QBClientError Int)

    -- ** Torrent Properties
  , -- | Get general properties of a torrent
    getTorrentProperties :: InfoHash -> IO (Either QBClientError TorrentProperties)
  , -- | Get trackers for a torrent
    getTorrentTrackers :: InfoHash -> IO (Either QBClientError [TorrentTracker])
  , -- | Get web seeds for a torrent
    getTorrentWebSeeds :: InfoHash -> IO (Either QBClientError [TorrentWebSeed])
  , -- | Get piece states (0=not downloaded, 1=downloading, 2=downloaded)
    getTorrentPieceStates :: InfoHash -> IO (Either QBClientError [Int])
  , -- | Get piece hashes for a torrent
    getTorrentPieceHashes :: InfoHash -> IO (Either QBClientError [Text])
  , -- | Export a torrent as .torrent file
    exportTorrent :: InfoHash -> IO (Either QBClientError ByteString)

    -- ** Torrent Actions
  , -- | Recheck torrents
    recheckTorrents :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Reannounce torrents to trackers
    reannounceTorrents :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Rename a torrent
    renameTorrent :: InfoHash -> Text -> IO (Either QBClientError ())
  , -- | Rename a file within a torrent
    renameFile :: InfoHash -> Text -> Text -> IO (Either QBClientError ())
  , -- | Rename a folder within a torrent
    renameFolder :: InfoHash -> Text -> Text -> IO (Either QBClientError ())
  , -- | Set save location for torrents
    setLocation :: [InfoHash] -> Text -> IO (Either QBClientError ())

    -- ** Priority Management
  , -- | Increase priority of torrents
    increasePriority :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Decrease priority of torrents
    decreasePriority :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Set torrents to maximum priority
    setTopPriority :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Set torrents to minimum priority
    setBottomPriority :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Set file priority (0=skip, 1=normal, 6=high, 7=maximal)
    setFilePriority :: InfoHash -> [Int] -> Int -> IO (Either QBClientError ())

    -- ** Speed Limits
  , -- | Set download limit for torrents (bytes/s, -1 for unlimited)
    setTorrentDownloadLimit :: [InfoHash] -> Int -> IO (Either QBClientError ())
  , -- | Set upload limit for torrents (bytes/s, -1 for unlimited)
    setTorrentUploadLimit :: [InfoHash] -> Int -> IO (Either QBClientError ())
  , -- | Get download limits for torrents
    getTorrentDownloadLimits :: [InfoHash] -> IO (Either QBClientError (Map InfoHash Int))
  , -- | Get upload limits for torrents
    getTorrentUploadLimits :: [InfoHash] -> IO (Either QBClientError (Map InfoHash Int))
  , -- | Set share limits for torrents
    setTorrentShareLimits :: [InfoHash] -> Double -> Int -> Int -> IO (Either QBClientError ())

    -- ** Torrent Behavior
  , -- | Enable/disable super seeding mode
    setSuperSeeding :: [InfoHash] -> Bool -> IO (Either QBClientError ())
  , -- | Enable/disable force start
    setForceStart :: [InfoHash] -> Bool -> IO (Either QBClientError ())
  , -- | Enable/disable automatic torrent management
    setAutoManagement :: [InfoHash] -> Bool -> IO (Either QBClientError ())
  , -- | Toggle sequential download mode
    toggleSequentialDownload :: [InfoHash] -> IO (Either QBClientError ())
  , -- | Toggle first/last piece priority
    toggleFirstLastPiecePriority :: [InfoHash] -> IO (Either QBClientError ())

    -- ** Category Management
  , -- | Get all categories
    getCategories :: IO (Either QBClientError (Map Text Category))
  , -- | Set category for torrents
    setTorrentCategory :: [InfoHash] -> Text -> IO (Either QBClientError ())
  , -- | Create a new category
    createCategory :: Text -> Text -> IO (Either QBClientError ())
  , -- | Edit an existing category
    editCategory :: Text -> Text -> IO (Either QBClientError ())
  , -- | Remove categories
    removeCategories :: [Text] -> IO (Either QBClientError ())

    -- ** Tag Management
  , -- | Get all tags
    getTags :: IO (Either QBClientError [Tag])
  , -- | Add tags to torrents
    addTags :: [InfoHash] -> [Tag] -> IO (Either QBClientError ())
  , -- | Remove tags from torrents
    removeTags :: [InfoHash] -> [Tag] -> IO (Either QBClientError ())
  , -- | Create new global tags
    createGlobalTags :: [Tag] -> IO (Either QBClientError ())
  , -- | Delete global tags
    deleteGlobalTags :: [Tag] -> IO (Either QBClientError ())

    -- ** Tracker Management
  , -- | Add trackers to a torrent
    addTorrentTrackers :: InfoHash -> [Text] -> IO (Either QBClientError ())
  , -- | Edit a tracker URL
    editTorrentTracker :: InfoHash -> Text -> Text -> IO (Either QBClientError ())
  , -- | Remove trackers from a torrent
    removeTorrentTrackers :: InfoHash -> [Text] -> IO (Either QBClientError ())
  , -- | Add peers to torrents
    addTorrentPeers :: [InfoHash] -> [Text] -> IO (Either QBClientError ())

    -- ** Log API
  , -- | Get main log entries
    getMainLog :: Bool -> Bool -> Bool -> Bool -> Maybe Int64 -> IO (Either QBClientError [LogEntry])
  , -- | Get peer log entries
    getPeersLog :: Maybe Int64 -> IO (Either QBClientError [PeerLogEntry])

    -- ** Sync API
  , -- | Get sync maindata (pass 0 for first request)
    syncMaindata :: Int64 -> IO (Either QBClientError SyncMainData)
  , -- | Get torrent peers with incremental updates
    syncTorrentPeers :: InfoHash -> Maybe Int64 -> IO (Either QBClientError SyncTorrentPeers)

    -- ** Transfer API
  , -- | Get global transfer info
    getTransferInfo :: IO (Either QBClientError TransferInfo)
  , -- | Get alternative speed limits mode
    getSpeedLimitsMode :: IO (Either QBClientError Bool)
  , -- | Toggle alternative speed limits
    toggleSpeedLimitsMode :: IO (Either QBClientError ())
  , -- | Get global download limit (bytes/s, 0 for unlimited)
    getGlobalDownloadLimit :: IO (Either QBClientError Int)
  , -- | Set global download limit (0 for unlimited)
    setGlobalDownloadLimit :: Int -> IO (Either QBClientError ())
  , -- | Get global upload limit (bytes/s, 0 for unlimited)
    getGlobalUploadLimit :: IO (Either QBClientError Int)
  , -- | Set global upload limit (0 for unlimited)
    setGlobalUploadLimit :: Int -> IO (Either QBClientError ())
  , -- | Ban peers permanently
    banPeers :: [Text] -> IO (Either QBClientError ())

    -- ** Session Management
  , -- | Logout from qBittorrent
    logout :: IO (Either QBClientError ())
  }

-- | Create a new qBittorrent client with automatic TLS manager
--
-- This function creates an HTTP manager internally and logs in automatically.
-- The session is maintained for the lifetime of the client.
--
-- @
-- client <- QB.initQBClient defaultConfig
-- torrents <- client.getTorrents Nothing
-- @
initQBClient :: QBConfig -> IO (Either QBClientError Client)
initQBClient config = do
  qbClient <- QB.initQBClient config
  loginResult <- QB.runQB qbClient (QB.login config)
  case loginResult of
    Left err -> pure $ Left (clientErrorToQBClientError err)
    Right response
      | response == "Ok." -> pure $ Right (mkClient qbClient)
      | otherwise -> pure $ Left (QBApiError QBResponseError{statusCode = 401, responseBody = "Login failed: " <> response})

-- | Create a new qBittorrent client with a provided HTTP manager
--
-- Use this when you want to share a manager across multiple clients
-- or need custom manager settings.
initQBClientWith :: Manager -> QBConfig -> IO (Either QBClientError Client)
initQBClientWith manager config = do
  qbClient <- QB.initQBClientWith manager config
  loginResult <- QB.runQB qbClient (QB.login config)
  case loginResult of
    Left err -> pure $ Left (clientErrorToQBClientError err)
    Right response
      | response == "Ok." -> pure $ Right (mkClient qbClient)
      | otherwise -> pure $ Left (QBApiError QBResponseError{statusCode = 401, responseBody = "Login failed: " <> response})

-- | Internal: Create the client record from QBClient
mkClient :: QB.QBClient -> Client
mkClient qbc = Client
  { -- App
    getVersion = run QB.getVersion
  , getWebapiVersion = run QB.getWebapiVersion
  , getBuildInfo = run QB.getBuildInfo
  , shutdownApp = runUnit QB.shutdownApp
  , getPreferences = run QB.getPreferences
  , setPreferences = \p -> runUnit (QB.setPreferences p)
  , getDefaultSavePath = run QB.getDefaultSavePath
  , getNetworkInterfaces = run QB.getNetworkInterfaces
  , getNetworkInterfaceAddresses = \i -> run (QB.getNetworkInterfaceAddresses i)

    -- Torrent Operations
  , addTorrent = \req -> run (QB.addTorrent req)
  , getTorrents = \mReq -> run (QB.getTorrents mReq)
  , getTorrentContents = \h -> run (QB.getTorrentContents h)
  , stopTorrents = \hs -> runUnit (QB.stopTorrents hs)
  , startTorrents = \hs -> runUnit (QB.startTorrents hs)
  , deleteTorrents = \hs df -> runUnit (QB.deleteTorrents hs df)
  , getTorrentsCount = run QB.getTorrentsCount

    -- Torrent Properties
  , getTorrentProperties = \h -> run (QB.getTorrentProperties h)
  , getTorrentTrackers = \h -> run (QB.getTorrentTrackers h)
  , getTorrentWebSeeds = \h -> run (QB.getTorrentWebSeeds h)
  , getTorrentPieceStates = \h -> run (QB.getTorrentPieceStates h)
  , getTorrentPieceHashes = \h -> run (QB.getTorrentPieceHashes h)
  , exportTorrent = \h -> run (QB.exportTorrent h)

    -- Torrent Actions
  , recheckTorrents = \hs -> runUnit (QB.recheckTorrents hs)
  , reannounceTorrents = \hs -> runUnit (QB.reannounceTorrents hs)
  , renameTorrent = \h n -> runUnit (QB.renameTorrent h n)
  , renameFile = \h o n -> runUnit (QB.renameFile h o n)
  , renameFolder = \h o n -> runUnit (QB.renameFolder h o n)
  , setLocation = \hs l -> runUnit (QB.setLocation hs l)

    -- Priority
  , increasePriority = \hs -> runUnit (QB.increasePriority hs)
  , decreasePriority = \hs -> runUnit (QB.decreasePriority hs)
  , setTopPriority = \hs -> runUnit (QB.setTopPriority hs)
  , setBottomPriority = \hs -> runUnit (QB.setBottomPriority hs)
  , setFilePriority = \h fs p -> runUnit (QB.setFilePriority h fs p)

    -- Speed Limits
  , setTorrentDownloadLimit = \hs l -> runUnit (QB.setTorrentDownloadLimit hs l)
  , setTorrentUploadLimit = \hs l -> runUnit (QB.setTorrentUploadLimit hs l)
  , getTorrentDownloadLimits = \hs -> run (QB.getTorrentDownloadLimits hs)
  , getTorrentUploadLimits = \hs -> run (QB.getTorrentUploadLimits hs)
  , setTorrentShareLimits = \hs r s i -> runUnit (QB.setTorrentShareLimits hs r s i)

    -- Behavior
  , setSuperSeeding = \hs e -> runUnit (QB.setSuperSeeding hs e)
  , setForceStart = \hs e -> runUnit (QB.setForceStart hs e)
  , setAutoManagement = \hs e -> runUnit (QB.setAutoManagement hs e)
  , toggleSequentialDownload = \hs -> runUnit (QB.toggleSequentialDownload hs)
  , toggleFirstLastPiecePriority = \hs -> runUnit (QB.toggleFirstLastPiecePriority hs)

    -- Categories
  , getCategories = run QB.getCategories
  , setTorrentCategory = \hs c -> runUnit (QB.setTorrentCategory hs c)
  , createCategory = \c s -> runUnit (QB.createCategory c s)
  , editCategory = \c s -> runUnit (QB.editCategory c s)
  , removeCategories = \cs -> runUnit (QB.removeCategories cs)

    -- Tags
  , getTags = run QB.getTags
  , addTags = \hs ts -> runUnit (QB.addTags hs ts)
  , removeTags = \hs ts -> runUnit (QB.removeTags hs ts)
  , createGlobalTags = \ts -> runUnit (QB.createGlobalTags ts)
  , deleteGlobalTags = \ts -> runUnit (QB.deleteGlobalTags ts)

    -- Trackers
  , addTorrentTrackers = \h us -> runUnit (QB.addTorrentTrackers h us)
  , editTorrentTracker = \h o n -> runUnit (QB.editTorrentTracker h o n)
  , removeTorrentTrackers = \h us -> runUnit (QB.removeTorrentTrackers h us)
  , addTorrentPeers = \hs ps -> runUnit (QB.addTorrentPeers hs ps)

    -- Log
  , getMainLog = \n i w c l -> run (QB.getMainLog n i w c l)
  , getPeersLog = \l -> run (QB.getPeersLog l)

    -- Sync
  , syncMaindata = \rid -> run (QB.syncMaindata rid)
  , syncTorrentPeers = \h rid -> run (QB.syncTorrentPeers h rid)

    -- Transfer
  , getTransferInfo = run QB.getTransferInfo
  , getSpeedLimitsMode = run QB.getSpeedLimitsMode
  , toggleSpeedLimitsMode = runUnit QB.toggleSpeedLimitsMode
  , getGlobalDownloadLimit = run QB.getGlobalDownloadLimit
  , setGlobalDownloadLimit = \l -> runUnit (QB.setGlobalDownloadLimit l)
  , getGlobalUploadLimit = run QB.getGlobalUploadLimit
  , setGlobalUploadLimit = \l -> runUnit (QB.setGlobalUploadLimit l)
  , banPeers = \ps -> runUnit (QB.banPeers ps)

    -- Session
  , logout = runUnit QB.logout
  }
  where
    run :: QB.ClientM a -> IO (Either QBClientError a)
    run action = first clientErrorToQBClientError <$> QB.runQB qbc action

    runUnit :: QB.ClientM NoContent -> IO (Either QBClientError ())
    runUnit action = first clientErrorToQBClientError . void <$> QB.runQB qbc action

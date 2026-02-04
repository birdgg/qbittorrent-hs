-- | Simplified record-based qBittorrent client
--
-- This module provides a record-style API where each operation is a simple
-- @IO (Either QBError a)@ function. This is more convenient for direct usage
-- compared to the servant-client @ClientM@ monad.
--
-- = Usage
--
-- @
-- import Network.QBittorrent.SimpleClient
--
-- main :: IO ()
-- main = do
--   -- Create client (handles login automatically)
--   client <- newQBittorrentClient defaultConfig
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
    QBittorrentClient (..)

    -- * Client Creation
  , newQBittorrentClient
  , newQBittorrentClientWith

    -- * Re-exports
  , module Network.QBittorrent.Types
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.QBittorrent.Client qualified as QB
import Network.QBittorrent.Types
import Servant.API (NoContent)

-- | Simplified qBittorrent client with record-based API
--
-- Each field is an IO action that returns @Either QBError a@.
-- The client manages session cookies automatically.
data QBittorrentClient = QBittorrentClient
  { -- | Get qBittorrent application version
    getVersion :: IO (Either QBError Text)
  , -- | Get WebAPI version
    getWebapiVersion :: IO (Either QBError Text)
  , -- | Get build info
    getBuildInfo :: IO (Either QBError BuildInfo)
  , -- | Shutdown qBittorrent application
    shutdownApp :: IO (Either QBError ())
  , -- | Get qBittorrent preferences
    getPreferences :: IO (Either QBError Preferences)
  , -- | Set qBittorrent preferences
    setPreferences :: Preferences -> IO (Either QBError ())
  , -- | Get default save path
    getDefaultSavePath :: IO (Either QBError Text)
  , -- | Get list of network interfaces
    getNetworkInterfaces :: IO (Either QBError [Text])
  , -- | Get addresses for a network interface
    getNetworkInterfaceAddresses :: Text -> IO (Either QBError [Text])

    -- ** Torrent Operations
  , -- | Add torrent by URL or magnet link
    addTorrent :: AddTorrentRequest -> IO (Either QBError Text)
  , -- | Get torrents info (pass Nothing to get all)
    getTorrents :: Maybe TorrentInfoRequest -> IO (Either QBError [TorrentInfo])
  , -- | Get files within a torrent
    getTorrentFiles :: Text -> IO (Either QBError [TorrentFile])
  , -- | Stop torrents by hash
    stopTorrents :: [Text] -> IO (Either QBError ())
  , -- | Start torrents by hash
    startTorrents :: [Text] -> IO (Either QBError ())
  , -- | Delete torrents (second param: delete files)
    deleteTorrents :: [Text] -> Bool -> IO (Either QBError ())
  , -- | Get total number of torrents
    getTorrentsCount :: IO (Either QBError Int)

    -- ** Torrent Properties
  , -- | Get general properties of a torrent
    getTorrentProperties :: Text -> IO (Either QBError TorrentProperties)
  , -- | Get trackers for a torrent
    getTorrentTrackers :: Text -> IO (Either QBError [TorrentTracker])
  , -- | Get web seeds for a torrent
    getTorrentWebSeeds :: Text -> IO (Either QBError [TorrentWebSeed])
  , -- | Get piece states (0=not downloaded, 1=downloading, 2=downloaded)
    getTorrentPieceStates :: Text -> IO (Either QBError [Int])
  , -- | Get piece hashes for a torrent
    getTorrentPieceHashes :: Text -> IO (Either QBError [Text])
  , -- | Export a torrent as .torrent file
    exportTorrent :: Text -> IO (Either QBError ByteString)

    -- ** Torrent Actions
  , -- | Recheck torrents
    recheckTorrents :: [Text] -> IO (Either QBError ())
  , -- | Reannounce torrents to trackers
    reannounceTorrents :: [Text] -> IO (Either QBError ())
  , -- | Rename a torrent
    renameTorrent :: Text -> Text -> IO (Either QBError ())
  , -- | Rename a file within a torrent
    renameFile :: Text -> Text -> Text -> IO (Either QBError ())
  , -- | Rename a folder within a torrent
    renameFolder :: Text -> Text -> Text -> IO (Either QBError ())
  , -- | Set save location for torrents
    setLocation :: [Text] -> Text -> IO (Either QBError ())

    -- ** Priority Management
  , -- | Increase priority of torrents
    increasePriority :: [Text] -> IO (Either QBError ())
  , -- | Decrease priority of torrents
    decreasePriority :: [Text] -> IO (Either QBError ())
  , -- | Set torrents to maximum priority
    setTopPriority :: [Text] -> IO (Either QBError ())
  , -- | Set torrents to minimum priority
    setBottomPriority :: [Text] -> IO (Either QBError ())
  , -- | Set file priority (0=skip, 1=normal, 6=high, 7=maximal)
    setFilePriority :: Text -> [Int] -> Int -> IO (Either QBError ())

    -- ** Speed Limits
  , -- | Set download limit for torrents (bytes/s, -1 for unlimited)
    setTorrentDownloadLimit :: [Text] -> Int -> IO (Either QBError ())
  , -- | Set upload limit for torrents (bytes/s, -1 for unlimited)
    setTorrentUploadLimit :: [Text] -> Int -> IO (Either QBError ())
  , -- | Get download limits for torrents
    getTorrentDownloadLimits :: [Text] -> IO (Either QBError (Map Text Int))
  , -- | Get upload limits for torrents
    getTorrentUploadLimits :: [Text] -> IO (Either QBError (Map Text Int))
  , -- | Set share limits for torrents
    setTorrentShareLimits :: [Text] -> Double -> Int -> Int -> IO (Either QBError ())

    -- ** Torrent Behavior
  , -- | Enable/disable super seeding mode
    setSuperSeeding :: [Text] -> Bool -> IO (Either QBError ())
  , -- | Enable/disable force start
    setForceStart :: [Text] -> Bool -> IO (Either QBError ())
  , -- | Enable/disable automatic torrent management
    setAutoManagement :: [Text] -> Bool -> IO (Either QBError ())
  , -- | Toggle sequential download mode
    toggleSequentialDownload :: [Text] -> IO (Either QBError ())
  , -- | Toggle first/last piece priority
    toggleFirstLastPiecePriority :: [Text] -> IO (Either QBError ())

    -- ** Category Management
  , -- | Get all categories
    getCategories :: IO (Either QBError (Map Text Category))
  , -- | Set category for torrents
    setTorrentCategory :: [Text] -> Text -> IO (Either QBError ())
  , -- | Create a new category
    createCategory :: Text -> Text -> IO (Either QBError ())
  , -- | Edit an existing category
    editCategory :: Text -> Text -> IO (Either QBError ())
  , -- | Remove categories
    removeCategories :: [Text] -> IO (Either QBError ())

    -- ** Tag Management
  , -- | Get all tags
    getTags :: IO (Either QBError [Text])
  , -- | Add tags to torrents
    addTags :: [Text] -> [Text] -> IO (Either QBError ())
  , -- | Remove tags from torrents
    removeTags :: [Text] -> [Text] -> IO (Either QBError ())
  , -- | Create new global tags
    createGlobalTags :: [Text] -> IO (Either QBError ())
  , -- | Delete global tags
    deleteGlobalTags :: [Text] -> IO (Either QBError ())

    -- ** Tracker Management
  , -- | Add trackers to a torrent
    addTorrentTrackers :: Text -> [Text] -> IO (Either QBError ())
  , -- | Edit a tracker URL
    editTorrentTracker :: Text -> Text -> Text -> IO (Either QBError ())
  , -- | Remove trackers from a torrent
    removeTorrentTrackers :: Text -> [Text] -> IO (Either QBError ())
  , -- | Add peers to torrents
    addTorrentPeers :: [Text] -> [Text] -> IO (Either QBError ())

    -- ** Log API
  , -- | Get main log entries
    getMainLog :: Bool -> Bool -> Bool -> Bool -> Maybe Int64 -> IO (Either QBError [LogEntry])
  , -- | Get peer log entries
    getPeersLog :: Maybe Int64 -> IO (Either QBError [PeerLogEntry])

    -- ** Sync API
  , -- | Get sync maindata (pass 0 for first request)
    syncMaindata :: Int64 -> IO (Either QBError SyncMainData)
  , -- | Get torrent peers with incremental updates
    syncTorrentPeers :: Text -> Maybe Int64 -> IO (Either QBError SyncTorrentPeers)

    -- ** Transfer API
  , -- | Get global transfer info
    getTransferInfo :: IO (Either QBError TransferInfo)
  , -- | Get alternative speed limits mode
    getSpeedLimitsMode :: IO (Either QBError Bool)
  , -- | Toggle alternative speed limits
    toggleSpeedLimitsMode :: IO (Either QBError ())
  , -- | Get global download limit (bytes/s, 0 for unlimited)
    getGlobalDownloadLimit :: IO (Either QBError Int)
  , -- | Set global download limit (0 for unlimited)
    setGlobalDownloadLimit :: Int -> IO (Either QBError ())
  , -- | Get global upload limit (bytes/s, 0 for unlimited)
    getGlobalUploadLimit :: IO (Either QBError Int)
  , -- | Set global upload limit (0 for unlimited)
    setGlobalUploadLimit :: Int -> IO (Either QBError ())
  , -- | Ban peers permanently
    banPeers :: [Text] -> IO (Either QBError ())

    -- ** Session Management
  , -- | Logout from qBittorrent
    logout :: IO (Either QBError ())
  }

-- | Create a new qBittorrent client with automatic TLS manager
--
-- This function creates an HTTP manager internally and logs in automatically.
-- The session is maintained for the lifetime of the client.
--
-- @
-- client <- newQBittorrentClient defaultConfig
-- torrents <- client.getTorrents Nothing
-- @
newQBittorrentClient :: QBConfig -> IO (Either QBError QBittorrentClient)
newQBittorrentClient config = do
  qbClient <- QB.newClient config
  loginAndMakeClient qbClient config

-- | Create a new qBittorrent client with a provided HTTP manager
--
-- Use this when you want to share a manager across multiple clients
-- or need custom manager settings.
newQBittorrentClientWith :: Manager -> QBConfig -> IO (Either QBError QBittorrentClient)
newQBittorrentClientWith manager config = do
  qbClient <- QB.newClientWith manager config
  loginAndMakeClient qbClient config

-- | Internal: Attempt login and create client record
loginAndMakeClient :: QB.QBClient -> QBConfig -> IO (Either QBError QBittorrentClient)
loginAndMakeClient qbClient config = do
  loginResult <- QB.runQB qbClient (QB.login config)
  case loginResult of
    Left err -> pure $ Left (clientErrorToQBError err)
    Right response
      | response == "Ok." -> pure $ Right (mkClient qbClient)
      | otherwise -> pure $ Left (AuthError $ "Login failed: " <> response)

-- | Internal: Create the client record from QBClient
mkClient :: QB.QBClient -> QBittorrentClient
mkClient qbc = QBittorrentClient
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
  , getTorrentFiles = \h -> run (QB.getTorrentFiles h)
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
  , syncMaindata = \r -> run (QB.syncMaindata r)
  , syncTorrentPeers = \h r -> run (QB.syncTorrentPeers h r)

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
    run :: QB.ClientM a -> IO (Either QBError a)
    run action = do
      result <- QB.runQB qbc action
      pure $ case result of
        Left err -> Left (clientErrorToQBError err)
        Right a -> Right a

    runUnit :: QB.ClientM NoContent -> IO (Either QBError ())
    runUnit action = do
      result <- QB.runQB qbc action
      pure $ case result of
        Left err -> Left (clientErrorToQBError err)
        Right _ -> Right ()

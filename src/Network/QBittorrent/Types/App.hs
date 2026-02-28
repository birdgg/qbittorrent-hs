-- | App types for qBittorrent API
module Network.QBittorrent.Types.App
  ( BuildInfo (..)
  , Preferences (..)
  , defaultPreferences
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Options (..)
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  , withObject
  , (.:)
  )
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Build information returned by /app/buildInfo
data BuildInfo = BuildInfo
  { qt :: Text
  , libtorrent :: Text
  , boost :: Text
  , openssl :: Text
  , bitness :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BuildInfo where
  parseJSON = withObject "BuildInfo" $ \o ->
    BuildInfo
      <$> o .: "qt"
      <*> o .: "libtorrent"
      <*> o .: "boost"
      <*> o .: "openssl"
      <*> o .: "bitness"

-- | JSON options for Preferences (camelCase to snake_case)
prefOptions :: Options
prefOptions = defaultOptions
  { fieldLabelModifier = camelToSnake
  , omitNothingFields = True
  }
  where
    camelToSnake :: String -> String
    camelToSnake = concatMap (\c -> if c >= 'A' && c <= 'Z' then ['_', toLower c] else [c])

-- | qBittorrent application preferences
--
-- All fields are optional (Maybe) for compatibility across qBittorrent versions
-- and to support partial updates when setting preferences.
data Preferences = Preferences
  { -- Core Settings
    locale :: Maybe Text
  , createSubfolderEnabled :: Maybe Bool
  , startPausedEnabled :: Maybe Bool
  , autoDeleteMode :: Maybe Int
  , preallocateAll :: Maybe Bool

    -- Download/Upload Paths
  , savePath :: Maybe Text
  , tempPathEnabled :: Maybe Bool
  , tempPath :: Maybe Text
  , incompleteFilesExt :: Maybe Bool
  , exportDir :: Maybe Text
  , exportDirFin :: Maybe Text

    -- Speed Limits
  , dlLimit :: Maybe Int
  , upLimit :: Maybe Int
  , altDlLimit :: Maybe Int
  , altUpLimit :: Maybe Int
  , limitUtpRate :: Maybe Bool
  , limitTcpOverhead :: Maybe Bool
  , limitLanPeers :: Maybe Bool

    -- Automatic Torrent Management
  , autoTmmEnabled :: Maybe Bool
  , torrentChangedTmmEnabled :: Maybe Bool
  , savePathChangedTmmEnabled :: Maybe Bool
  , categoryChangedTmmEnabled :: Maybe Bool

    -- Queueing
  , queueingEnabled :: Maybe Bool
  , maxActiveDownloads :: Maybe Int
  , maxActiveTorrents :: Maybe Int
  , maxActiveUploads :: Maybe Int
  , dontCountSlowTorrents :: Maybe Bool
  , slowTorrentDlRateThreshold :: Maybe Int
  , slowTorrentUlRateThreshold :: Maybe Int
  , slowTorrentInactiveTimer :: Maybe Int

    -- Share Ratio
  , maxRatioEnabled :: Maybe Bool
  , maxRatio :: Maybe Double
  , maxRatioAct :: Maybe Int
  , maxSeedingTimeEnabled :: Maybe Bool
  , maxSeedingTime :: Maybe Int

    -- Network
  , listenPort :: Maybe Int
  , randomPort :: Maybe Bool
  , upnp :: Maybe Bool
  , maxConnec :: Maybe Int
  , maxConnecPerTorrent :: Maybe Int
  , maxUploads :: Maybe Int
  , maxUploadsPerTorrent :: Maybe Int
  , stopTrackerTimeout :: Maybe Int
  , enablePieceExtentAffinity :: Maybe Bool
  , bittorrentProtocol :: Maybe Int

    -- Protocol & Discovery
  , dht :: Maybe Bool
  , pex :: Maybe Bool
  , lsd :: Maybe Bool
  , encryption :: Maybe Int
  , anonymousMode :: Maybe Bool

    -- Proxy
  , proxyType :: Maybe Int
  , proxyIp :: Maybe Text
  , proxyPort :: Maybe Int
  , proxyAuthEnabled :: Maybe Bool
  , proxyUsername :: Maybe Text
  , proxyPassword :: Maybe Text
  , proxyPeerConnections :: Maybe Bool
  , proxyTorrentsOnly :: Maybe Bool

    -- WebUI
  , webUiAddress :: Maybe Text
  , webUiPort :: Maybe Int
  , webUiUpnp :: Maybe Bool
  , webUiUsername :: Maybe Text
  , webUiPassword :: Maybe Text
  , webUiCsrfProtectionEnabled :: Maybe Bool
  , webUiClickjackingProtectionEnabled :: Maybe Bool
  , webUiSecureCookieEnabled :: Maybe Bool
  , webUiMaxAuthFailCount :: Maybe Int
  , webUiBanDuration :: Maybe Int
  , webUiSessionTimeout :: Maybe Int
  , webUiHostHeaderValidationEnabled :: Maybe Bool
  , bypassLocalAuth :: Maybe Bool
  , bypassAuthSubnetWhitelistEnabled :: Maybe Bool
  , bypassAuthSubnetWhitelist :: Maybe Text
  , webUiDomainList :: Maybe Text
  , alternativeWebuiEnabled :: Maybe Bool
  , alternativeWebuiPath :: Maybe Text
  , useHttps :: Maybe Bool
  , webUiHttpsKeyPath :: Maybe Text
  , webUiHttpsCertPath :: Maybe Text
  , webUiUseCustomHttpHeadersEnabled :: Maybe Bool
  , webUiCustomHttpHeaders :: Maybe Text

    -- IP Filtering
  , ipFilterEnabled :: Maybe Bool
  , ipFilterPath :: Maybe Text
  , ipFilterTrackers :: Maybe Bool
  , bannedIPs :: Maybe Text

    -- Email Notifications
  , mailNotificationEnabled :: Maybe Bool
  , mailNotificationSender :: Maybe Text
  , mailNotificationEmail :: Maybe Text
  , mailNotificationSmtp :: Maybe Text
  , mailNotificationSslEnabled :: Maybe Bool
  , mailNotificationAuthEnabled :: Maybe Bool
  , mailNotificationUsername :: Maybe Text
  , mailNotificationPassword :: Maybe Text

    -- RSS
  , rssRefreshInterval :: Maybe Int
  , rssMaxArticlesPerFeed :: Maybe Int
  , rssProcessingEnabled :: Maybe Bool
  , rssAutoDownloadingEnabled :: Maybe Bool
  , rssDownloadRepackProperEpisodes :: Maybe Bool
  , rssSmartEpisodeFilters :: Maybe Text

    -- Trackers & Automation
  , addTrackersEnabled :: Maybe Bool
  , addTrackers :: Maybe Text
  , scanDirs :: Maybe (HashMap Text Int)
  , autorunEnabled :: Maybe Bool
  , autorunProgram :: Maybe Text

    -- Advanced
  , announceIp :: Maybe Text
  , announceToAllTiers :: Maybe Bool
  , announceToAllTrackers :: Maybe Bool
  , asyncIoThreads :: Maybe Int
  , checkingMemoryUse :: Maybe Int
  , currentInterfaceAddress :: Maybe Text
  , currentNetworkInterface :: Maybe Text
  , diskCache :: Maybe Int
  , diskCacheTtl :: Maybe Int
  , embeddedTrackerPort :: Maybe Int
  , enableCoalesceReadWrite :: Maybe Bool
  , enableEmbeddedTracker :: Maybe Bool
  , enableMultiConnectionsFromSameIp :: Maybe Bool
  , enableOsCache :: Maybe Bool
  , enableUploadSuggestions :: Maybe Bool
  , filePoolSize :: Maybe Int
  , outgoingPortsMin :: Maybe Int
  , outgoingPortsMax :: Maybe Int
  , recheckCompletedTorrents :: Maybe Bool
  , resolvePeerCountries :: Maybe Bool
  , saveResumeDataInterval :: Maybe Int
  , sendBufferLowWatermark :: Maybe Int
  , sendBufferWatermark :: Maybe Int
  , sendBufferWatermarkFactor :: Maybe Int
  , socketBacklogSize :: Maybe Int
  , uploadChokingAlgorithm :: Maybe Int
  , uploadSlotsBehavior :: Maybe Int
  , upnpLeaseDuration :: Maybe Int
  , utpTcpMixedMode :: Maybe Int

    -- DDNS
  , dyndnsEnabled :: Maybe Bool
  , dyndnsService :: Maybe Int
  , dyndnsUsername :: Maybe Text
  , dyndnsPassword :: Maybe Text
  , dyndnsDomain :: Maybe Text

    -- Scheduler
  , schedulerEnabled :: Maybe Bool
  , scheduleFromHour :: Maybe Int
  , scheduleFromMin :: Maybe Int
  , scheduleToHour :: Maybe Int
  , scheduleToMin :: Maybe Int
  , schedulerDays :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Preferences where
  parseJSON = genericParseJSON prefOptions

instance ToJSON Preferences where
  toJSON = genericToJSON prefOptions

-- | Empty preferences (all fields Nothing)
defaultPreferences :: Preferences
defaultPreferences = Preferences
  { locale = Nothing
  , createSubfolderEnabled = Nothing
  , startPausedEnabled = Nothing
  , autoDeleteMode = Nothing
  , preallocateAll = Nothing
  , savePath = Nothing
  , tempPathEnabled = Nothing
  , tempPath = Nothing
  , incompleteFilesExt = Nothing
  , exportDir = Nothing
  , exportDirFin = Nothing
  , dlLimit = Nothing
  , upLimit = Nothing
  , altDlLimit = Nothing
  , altUpLimit = Nothing
  , limitUtpRate = Nothing
  , limitTcpOverhead = Nothing
  , limitLanPeers = Nothing
  , autoTmmEnabled = Nothing
  , torrentChangedTmmEnabled = Nothing
  , savePathChangedTmmEnabled = Nothing
  , categoryChangedTmmEnabled = Nothing
  , queueingEnabled = Nothing
  , maxActiveDownloads = Nothing
  , maxActiveTorrents = Nothing
  , maxActiveUploads = Nothing
  , dontCountSlowTorrents = Nothing
  , slowTorrentDlRateThreshold = Nothing
  , slowTorrentUlRateThreshold = Nothing
  , slowTorrentInactiveTimer = Nothing
  , maxRatioEnabled = Nothing
  , maxRatio = Nothing
  , maxRatioAct = Nothing
  , maxSeedingTimeEnabled = Nothing
  , maxSeedingTime = Nothing
  , listenPort = Nothing
  , randomPort = Nothing
  , upnp = Nothing
  , maxConnec = Nothing
  , maxConnecPerTorrent = Nothing
  , maxUploads = Nothing
  , maxUploadsPerTorrent = Nothing
  , stopTrackerTimeout = Nothing
  , enablePieceExtentAffinity = Nothing
  , bittorrentProtocol = Nothing
  , dht = Nothing
  , pex = Nothing
  , lsd = Nothing
  , encryption = Nothing
  , anonymousMode = Nothing
  , proxyType = Nothing
  , proxyIp = Nothing
  , proxyPort = Nothing
  , proxyAuthEnabled = Nothing
  , proxyUsername = Nothing
  , proxyPassword = Nothing
  , proxyPeerConnections = Nothing
  , proxyTorrentsOnly = Nothing
  , webUiAddress = Nothing
  , webUiPort = Nothing
  , webUiUpnp = Nothing
  , webUiUsername = Nothing
  , webUiPassword = Nothing
  , webUiCsrfProtectionEnabled = Nothing
  , webUiClickjackingProtectionEnabled = Nothing
  , webUiSecureCookieEnabled = Nothing
  , webUiMaxAuthFailCount = Nothing
  , webUiBanDuration = Nothing
  , webUiSessionTimeout = Nothing
  , webUiHostHeaderValidationEnabled = Nothing
  , bypassLocalAuth = Nothing
  , bypassAuthSubnetWhitelistEnabled = Nothing
  , bypassAuthSubnetWhitelist = Nothing
  , webUiDomainList = Nothing
  , alternativeWebuiEnabled = Nothing
  , alternativeWebuiPath = Nothing
  , useHttps = Nothing
  , webUiHttpsKeyPath = Nothing
  , webUiHttpsCertPath = Nothing
  , webUiUseCustomHttpHeadersEnabled = Nothing
  , webUiCustomHttpHeaders = Nothing
  , ipFilterEnabled = Nothing
  , ipFilterPath = Nothing
  , ipFilterTrackers = Nothing
  , bannedIPs = Nothing
  , mailNotificationEnabled = Nothing
  , mailNotificationSender = Nothing
  , mailNotificationEmail = Nothing
  , mailNotificationSmtp = Nothing
  , mailNotificationSslEnabled = Nothing
  , mailNotificationAuthEnabled = Nothing
  , mailNotificationUsername = Nothing
  , mailNotificationPassword = Nothing
  , rssRefreshInterval = Nothing
  , rssMaxArticlesPerFeed = Nothing
  , rssProcessingEnabled = Nothing
  , rssAutoDownloadingEnabled = Nothing
  , rssDownloadRepackProperEpisodes = Nothing
  , rssSmartEpisodeFilters = Nothing
  , addTrackersEnabled = Nothing
  , addTrackers = Nothing
  , scanDirs = Nothing
  , autorunEnabled = Nothing
  , autorunProgram = Nothing
  , announceIp = Nothing
  , announceToAllTiers = Nothing
  , announceToAllTrackers = Nothing
  , asyncIoThreads = Nothing
  , checkingMemoryUse = Nothing
  , currentInterfaceAddress = Nothing
  , currentNetworkInterface = Nothing
  , diskCache = Nothing
  , diskCacheTtl = Nothing
  , embeddedTrackerPort = Nothing
  , enableCoalesceReadWrite = Nothing
  , enableEmbeddedTracker = Nothing
  , enableMultiConnectionsFromSameIp = Nothing
  , enableOsCache = Nothing
  , enableUploadSuggestions = Nothing
  , filePoolSize = Nothing
  , outgoingPortsMin = Nothing
  , outgoingPortsMax = Nothing
  , recheckCompletedTorrents = Nothing
  , resolvePeerCountries = Nothing
  , saveResumeDataInterval = Nothing
  , sendBufferLowWatermark = Nothing
  , sendBufferWatermark = Nothing
  , sendBufferWatermarkFactor = Nothing
  , socketBacklogSize = Nothing
  , uploadChokingAlgorithm = Nothing
  , uploadSlotsBehavior = Nothing
  , upnpLeaseDuration = Nothing
  , utpTcpMixedMode = Nothing
  , dyndnsEnabled = Nothing
  , dyndnsService = Nothing
  , dyndnsUsername = Nothing
  , dyndnsPassword = Nothing
  , dyndnsDomain = Nothing
  , schedulerEnabled = Nothing
  , scheduleFromHour = Nothing
  , scheduleFromMin = Nothing
  , scheduleToHour = Nothing
  , scheduleToMin = Nothing
  , schedulerDays = Nothing
  }

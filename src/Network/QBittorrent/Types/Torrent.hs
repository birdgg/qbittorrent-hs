-- | Torrent-related types for qBittorrent API
module Network.QBittorrent.Types.Torrent
  ( -- * Torrent State
    TorrentState (..)

    -- * Torrent Info
  , TorrentInfo (..)

    -- * Torrent Contents
  , TorrentContent (..)

    -- * Torrent Properties
  , TorrentProperties (..)

    -- * Torrent Trackers
  , TorrentTracker (..)
  , TrackerStatus (..)

    -- * Torrent Web Seeds
  , TorrentWebSeed (..)

    -- * Categories
  , Category (..)

    -- * Request Types
  , TorrentInfoRequest (..)
  , AddTorrentRequest (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Filter (TorrentFilter)
import Network.QBittorrent.Types.InfoHash (InfoHash)
import Network.QBittorrent.Types.Tag (Tag, tagsToText, textToTags)
import Web.FormUrlEncoded (Form (..), ToForm (..))

import Data.Map.Strict qualified as Map

-- | Request parameters for getting torrent list
data TorrentInfoRequest = TorrentInfoRequest
  { filter_ :: Maybe TorrentFilter
  , category :: Maybe Text
  , tag :: Maybe Tag
  , hashes :: Maybe [InfoHash]
  }
  deriving stock (Show, Eq, Generic)

-- | Torrent information from qBittorrent
data TorrentInfo = TorrentInfo
  { -- Identification
    hash :: InfoHash
  , name :: Text
  , magnetUri :: Text
    -- State & Progress
  , state :: TorrentState
  , progress :: Double
  , eta :: Int64
  , size :: Int64
  , totalSize :: Int64
  , completed :: Int64
  , amountLeft :: Int64
  , availability :: Double
    -- Paths
  , savePath :: Text
  , contentPath :: Text
    -- Timestamps
  , addedOn :: Int64
  , completionOn :: Int64
  , lastActivity :: Int64
  , seenComplete :: Int64
  , timeActive :: Int64
    -- Download stats
  , downloaded :: Int64
  , downloadedSession :: Int64
  , dlspeed :: Int64
    -- Upload stats
  , uploaded :: Int64
  , uploadedSession :: Int64
  , upspeed :: Int64
    -- Ratio & Seeding
  , ratio :: Double
  , ratioLimit :: Double
  , maxRatio :: Double
  , seedingTime :: Int64
  , seedingTimeLimit :: Int64
  , maxSeedingTime :: Int64
    -- Speed limits
  , dlLimit :: Int64
  , upLimit :: Int64
    -- Peers / Swarm
  , numSeeds :: Int64
  , numComplete :: Int64
  , numLeechs :: Int64
  , numIncomplete :: Int64
    -- Queue
  , priority :: Int64
    -- Boolean flags
  , autoTmm :: Bool
  , flPiecePrio :: Bool
  , forceStart :: Bool
  , seqDl :: Bool
  , superSeeding :: Bool
  , isPrivate :: Bool
    -- Tracker
  , tracker :: Text
  , reannounce :: Int64
    -- Metadata
  , category :: Text
  , tags :: [Tag]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentInfo where
  parseJSON = withObject "TorrentInfo" $ \o ->
    TorrentInfo
      -- Identification
      <$> o .: "hash"
      <*> o .: "name"
      <*> o .:? "magnet_uri" .!= ""
      -- State & Progress
      <*> o .: "state"
      <*> o .: "progress"
      <*> o .: "eta"
      <*> o .: "size"
      <*> o .: "total_size"
      <*> o .: "completed"
      <*> o .: "amount_left"
      <*> o .: "availability"
      -- Paths
      <*> o .: "save_path"
      <*> o .: "content_path"
      -- Timestamps
      <*> o .: "added_on"
      <*> o .: "completion_on"
      <*> o .: "last_activity"
      <*> o .: "seen_complete"
      <*> o .: "time_active"
      -- Download stats
      <*> o .: "downloaded"
      <*> o .: "downloaded_session"
      <*> o .: "dlspeed"
      -- Upload stats
      <*> o .: "uploaded"
      <*> o .: "uploaded_session"
      <*> o .: "upspeed"
      -- Ratio & Seeding
      <*> o .: "ratio"
      <*> o .: "ratio_limit"
      <*> o .: "max_ratio"
      <*> o .: "seeding_time"
      <*> o .: "seeding_time_limit"
      <*> o .: "max_seeding_time"
      -- Speed limits
      <*> o .: "dl_limit"
      <*> o .: "up_limit"
      -- Peers / Swarm
      <*> o .: "num_seeds"
      <*> o .: "num_complete"
      <*> o .: "num_leechs"
      <*> o .: "num_incomplete"
      -- Queue
      <*> o .: "priority"
      -- Boolean flags
      <*> o .: "auto_tmm"
      <*> o .: "f_l_piece_prio"
      <*> o .: "force_start"
      <*> o .: "seq_dl"
      <*> o .: "super_seeding"
      <*> o .:? "isPrivate" .!= False
      -- Tracker
      <*> o .: "tracker"
      <*> o .: "reannounce"
      -- Metadata
      <*> o .:? "category" .!= ""
      <*> (textToTags <$> o .:? "tags" .!= "")

-- | File information within a torrent
data TorrentContent = TorrentContent
  { index :: Int
  , name :: Text
  , size :: Int64
  , progress :: Double
  , priority :: Int
  , isSeed :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentContent where
  parseJSON = withObject "TorrentContent" $ \o ->
    TorrentContent
      <$> o .: "index"
      <*> o .: "name"
      <*> o .: "size"
      <*> o .: "progress"
      <*> o .:? "priority" .!= 1
      <*> o .:? "is_seed" .!= False

-- | Request to add torrents via URLs
data AddTorrentRequest = AddTorrentRequest
  { urls :: Maybe Text
  , savepath :: Maybe Text
  , category :: Maybe Text
  , tags :: Maybe [Tag]
  , rename :: Maybe Text
  , stopped :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AddTorrentRequest where
  toJSON req =
    object $
      catMaybes
        [ ("urls" .=) <$> req.urls
        , ("savepath" .=) <$> req.savepath
        , ("category" .=) <$> req.category
        , fmap (\ts -> "tags" .= tagsToText ts) req.tags
        , ("rename" .=) <$> req.rename
        , ("stopped" .=) <$> req.stopped
        ]

instance ToForm AddTorrentRequest where
  toForm req =
    Form $
      Map.fromList $
        catMaybes
          [ fmap (\v -> ("urls", [v])) req.urls
          , fmap (\v -> ("savepath", [v])) req.savepath
          , fmap (\v -> ("category", [v])) req.category
          , fmap (\ts -> ("tags", [tagsToText ts])) req.tags
          , fmap (\v -> ("rename", [v])) req.rename
          , fmap (\v -> ("stopped", [if v then "true" else "false"])) req.stopped
          ]

-- | Torrent general properties
data TorrentProperties = TorrentProperties
  { savePath :: Text
  , creationDate :: Maybe Int64
  , pieceSize :: Maybe Int64
  , comment :: Maybe Text
  , totalWasted :: Maybe Int64
  , totalUploaded :: Maybe Int64
  , totalUploadedSession :: Maybe Int64
  , totalDownloaded :: Maybe Int64
  , totalDownloadedSession :: Maybe Int64
  , upLimit :: Maybe Int64
  , dlLimit :: Maybe Int64
  , timeElapsed :: Maybe Int64
  , seedingTime :: Maybe Int64
  , nbConnections :: Maybe Int
  , nbConnectionsLimit :: Maybe Int
  , shareRatio :: Maybe Double
  , additionDate :: Maybe Int64
  , completionDate :: Maybe Int64
  , createdBy :: Maybe Text
  , dlSpeedAvg :: Maybe Int64
  , dlSpeed :: Maybe Int64
  , eta :: Maybe Int64
  , lastSeen :: Maybe Int64
  , peers :: Maybe Int
  , peersTotal :: Maybe Int
  , piecesHave :: Maybe Int
  , piecesNum :: Maybe Int
  , reannounce :: Maybe Int64
  , seeds :: Maybe Int
  , seedsTotal :: Maybe Int
  , totalSize :: Maybe Int64
  , upSpeedAvg :: Maybe Int64
  , upSpeed :: Maybe Int64
  , isPrivate :: Maybe Bool  -- v5.0+ only
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentProperties where
  parseJSON = withObject "TorrentProperties" $ \o ->
    TorrentProperties
      <$> o .: "save_path"
      <*> o .:? "creation_date"
      <*> o .:? "piece_size"
      <*> o .:? "comment"
      <*> o .:? "total_wasted"
      <*> o .:? "total_uploaded"
      <*> o .:? "total_uploaded_session"
      <*> o .:? "total_downloaded"
      <*> o .:? "total_downloaded_session"
      <*> o .:? "up_limit"
      <*> o .:? "dl_limit"
      <*> o .:? "time_elapsed"
      <*> o .:? "seeding_time"
      <*> o .:? "nb_connections"
      <*> o .:? "nb_connections_limit"
      <*> o .:? "share_ratio"
      <*> o .:? "addition_date"
      <*> o .:? "completion_date"
      <*> o .:? "created_by"
      <*> o .:? "dl_speed_avg"
      <*> o .:? "dl_speed"
      <*> o .:? "eta"
      <*> o .:? "last_seen"
      <*> o .:? "peers"
      <*> o .:? "peers_total"
      <*> o .:? "pieces_have"
      <*> o .:? "pieces_num"
      <*> o .:? "reannounce"
      <*> o .:? "seeds"
      <*> o .:? "seeds_total"
      <*> o .:? "total_size"
      <*> o .:? "up_speed_avg"
      <*> o .:? "up_speed"
      <*> o .:? "is_private"

-- | Tracker status
data TrackerStatus
  = TrackerDisabled       -- 0
  | TrackerNotContacted   -- 1
  | TrackerWorking        -- 2
  | TrackerUpdating       -- 3
  | TrackerNotWorking     -- 4
  deriving stock (Show, Eq, Generic)

instance FromJSON TrackerStatus where
  parseJSON = withScientific "TrackerStatus" $ \n ->
    case round n :: Int of
      0 -> pure TrackerDisabled
      1 -> pure TrackerNotContacted
      2 -> pure TrackerWorking
      3 -> pure TrackerUpdating
      4 -> pure TrackerNotWorking
      _ -> fail "Unknown tracker status"

-- | Torrent tracker information
data TorrentTracker = TorrentTracker
  { url :: Text
  , status :: TrackerStatus
  , tier :: Maybe Int
  , numPeers :: Maybe Int
  , numSeeds :: Maybe Int
  , numLeeches :: Maybe Int
  , numDownloaded :: Maybe Int
  , msg :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentTracker where
  parseJSON = withObject "TorrentTracker" $ \o ->
    TorrentTracker
      <$> o .: "url"
      <*> o .: "status"
      <*> o .:? "tier"
      <*> o .:? "num_peers"
      <*> o .:? "num_seeds"
      <*> o .:? "num_leeches"
      <*> o .:? "num_downloaded"
      <*> o .:? "msg"

-- | Torrent web seed information
newtype TorrentWebSeed = TorrentWebSeed
  { url :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentWebSeed where
  parseJSON = withObject "TorrentWebSeed" $ \o ->
    TorrentWebSeed <$> o .: "url"

-- | Category information
data Category = Category
  { name :: Text
  , savePath :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Category where
  parseJSON = withObject "Category" $ \o ->
    Category
      <$> o .:? "name" .!= ""
      <*> o .:? "savePath" .!= ""

-- | Torrent state from qBittorrent 5.0+ API
data TorrentState
  = StateError          -- ^ error
  | MissingFiles        -- ^ missingFiles
  | Uploading           -- ^ uploading
  | PausedUP            -- ^ pausedUP (qBittorrent 4.x)
  | StoppedUP           -- ^ stoppedUP (qBittorrent 5.0+)
  | QueuedUP            -- ^ queuedUP
  | StalledUP           -- ^ stalledUP
  | CheckingUP          -- ^ checkingUP
  | ForcedUP            -- ^ forcedUP
  | Allocating          -- ^ allocating
  | Downloading         -- ^ downloading
  | MetaDL              -- ^ metaDL
  | PausedDL            -- ^ pausedDL (qBittorrent 4.x)
  | StoppedDL           -- ^ stoppedDL (qBittorrent 5.0+)
  | QueuedDL            -- ^ queuedDL
  | StalledDL           -- ^ stalledDL
  | CheckingDL          -- ^ checkingDL
  | ForcedDL            -- ^ forcedDL
  | CheckingResumeData  -- ^ checkingResumeData
  | Moving              -- ^ moving
  | StateUnknown        -- ^ unknown / fallback
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentState where
  parseJSON = withText "TorrentState" $ \case
    "error"              -> pure StateError
    "missingFiles"       -> pure MissingFiles
    "uploading"          -> pure Uploading
    "pausedUP"           -> pure PausedUP
    "stoppedUP"          -> pure StoppedUP
    "queuedUP"           -> pure QueuedUP
    "stalledUP"          -> pure StalledUP
    "checkingUP"         -> pure CheckingUP
    "forcedUP"           -> pure ForcedUP
    "allocating"         -> pure Allocating
    "downloading"        -> pure Downloading
    "metaDL"             -> pure MetaDL
    "pausedDL"           -> pure PausedDL
    "stoppedDL"          -> pure StoppedDL
    "queuedDL"           -> pure QueuedDL
    "stalledDL"          -> pure StalledDL
    "checkingDL"         -> pure CheckingDL
    "forcedDL"           -> pure ForcedDL
    "checkingResumeData" -> pure CheckingResumeData
    "moving"             -> pure Moving
    _                    -> pure StateUnknown

instance ToJSON TorrentState where
  toJSON = \case
    StateError         -> String "error"
    MissingFiles       -> String "missingFiles"
    Uploading          -> String "uploading"
    PausedUP           -> String "pausedUP"
    StoppedUP          -> String "stoppedUP"
    QueuedUP           -> String "queuedUP"
    StalledUP          -> String "stalledUP"
    CheckingUP         -> String "checkingUP"
    ForcedUP           -> String "forcedUP"
    Allocating         -> String "allocating"
    Downloading        -> String "downloading"
    MetaDL             -> String "metaDL"
    PausedDL           -> String "pausedDL"
    StoppedDL          -> String "stoppedDL"
    QueuedDL           -> String "queuedDL"
    StalledDL          -> String "stalledDL"
    CheckingDL         -> String "checkingDL"
    ForcedDL           -> String "forcedDL"
    CheckingResumeData -> String "checkingResumeData"
    Moving             -> String "moving"
    StateUnknown       -> String "unknown"

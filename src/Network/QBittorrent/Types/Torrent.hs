-- | Torrent-related types for qBittorrent API
module Network.QBittorrent.Types.Torrent
  ( -- * Torrent Info
    TorrentInfo (..)
  , isCompleted

    -- * Torrent Files
  , TorrentFile (..)

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
import Web.FormUrlEncoded (Form (..), ToForm (..))

import Data.Map.Strict qualified as Map

-- | Request parameters for getting torrent list
data TorrentInfoRequest = TorrentInfoRequest
  { filter_ :: Maybe TorrentFilter
  , category :: Maybe Text
  , tag :: Maybe Text
  , hashes :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Torrent information from qBittorrent
data TorrentInfo = TorrentInfo
  { hash :: Text
  , name :: Text
  , state :: Text
  , progress :: Double
  , savePath :: Text
  , size :: Int64
  , downloaded :: Int64
  , eta :: Int64
  , tags :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentInfo where
  parseJSON = withObject "TorrentInfo" $ \o ->
    TorrentInfo
      <$> o .: "hash"
      <*> o .: "name"
      <*> o .: "state"
      <*> o .: "progress"
      <*> o .: "save_path"
      <*> o .: "size"
      <*> o .: "downloaded"
      <*> o .: "eta"
      <*> o .:? "tags" .!= ""

-- | Check if the torrent download is completed
isCompleted :: TorrentInfo -> Bool
isCompleted t =
  t.progress >= 1.0
    || t.state == "uploading"
    || t.state == "stalledUP"
    || t.state == "stoppedUP"
    || t.state == "forcedUP"
    || t.state == "queuedUP"
    || t.state == "checkingUP"

-- | File information within a torrent
data TorrentFile = TorrentFile
  { index :: Int
  , name :: Text
  , size :: Int64
  , progress :: Double
  , priority :: Int
  , isSeed :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentFile where
  parseJSON = withObject "TorrentFile" $ \o ->
    TorrentFile
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
  , tags :: Maybe Text
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
        , ("tags" .=) <$> req.tags
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
          , fmap (\v -> ("tags", [v])) req.tags
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

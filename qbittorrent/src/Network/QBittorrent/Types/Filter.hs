-- | Torrent filter types for qBittorrent API
module Network.QBittorrent.Types.Filter
  ( TorrentFilter (..)
  , torrentFilterToText
  ) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Torrent state filter for qBittorrent 5.0+
data TorrentFilter
  = FilterAll
  | FilterDownloading
  | FilterSeeding
  | FilterCompleted
  | FilterStopped
  | FilterActive
  | FilterInactive
  | FilterRunning
  | FilterStalled
  | FilterStalledUploading
  | FilterStalledDownloading
  | FilterErrored
  deriving stock (Show, Eq, Generic)

-- | Convert filter to API string
torrentFilterToText :: TorrentFilter -> Text
torrentFilterToText = \case
  FilterAll -> "all"
  FilterDownloading -> "downloading"
  FilterSeeding -> "seeding"
  FilterCompleted -> "completed"
  FilterStopped -> "stopped"
  FilterActive -> "active"
  FilterInactive -> "inactive"
  FilterRunning -> "running"
  FilterStalled -> "stalled"
  FilterStalledUploading -> "stalled_uploading"
  FilterStalledDownloading -> "stalled_downloading"
  FilterErrored -> "errored"

instance ToJSON TorrentFilter where
  toJSON = String . torrentFilterToText

-- | Sync API types for qBittorrent
module Network.QBittorrent.Types.Sync
  ( SyncMainData (..)
  , SyncTorrentInfo (..)
  , ServerState (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Sync maindata response from qBittorrent
data SyncMainData = SyncMainData
  { rid :: Int64
  , fullUpdate :: Bool
  , torrents :: Map Text SyncTorrentInfo
  , torrentsRemoved :: [Text]
  , serverState :: Maybe ServerState
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SyncMainData where
  parseJSON = withObject "SyncMainData" $ \o ->
    SyncMainData
      <$> o .: "rid"
      <*> o .:? "full_update" .!= False
      <*> o .:? "torrents" .!= mempty
      <*> o .:? "torrents_removed" .!= []
      <*> o .:? "server_state"

-- | Partial torrent info for sync API
data SyncTorrentInfo = SyncTorrentInfo
  { name :: Maybe Text
  , state :: Maybe Text
  , progress :: Maybe Double
  , savePath :: Maybe Text
  , size :: Maybe Int64
  , downloaded :: Maybe Int64
  , eta :: Maybe Int64
  , dlspeed :: Maybe Int64
  , upspeed :: Maybe Int64
  , numSeeds :: Maybe Int64
  , numLeechs :: Maybe Int64
  , ratio :: Maybe Double
  , addedOn :: Maybe Int64
  , completionOn :: Maybe Int64
  , category :: Maybe Text
  , tags :: Maybe Text
  , contentPath :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SyncTorrentInfo where
  parseJSON = withObject "SyncTorrentInfo" $ \o ->
    SyncTorrentInfo
      <$> o .:? "name"
      <*> o .:? "state"
      <*> o .:? "progress"
      <*> o .:? "save_path"
      <*> o .:? "size"
      <*> o .:? "downloaded"
      <*> o .:? "eta"
      <*> o .:? "dlspeed"
      <*> o .:? "upspeed"
      <*> o .:? "num_seeds"
      <*> o .:? "num_leechs"
      <*> o .:? "ratio"
      <*> o .:? "added_on"
      <*> o .:? "completion_on"
      <*> o .:? "category"
      <*> o .:? "tags"
      <*> o .:? "content_path"

-- | Server state from sync maindata
data ServerState = ServerState
  { dlInfoSpeed :: Maybe Int64
  , upInfoSpeed :: Maybe Int64
  , dlInfoData :: Maybe Int64
  , upInfoData :: Maybe Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ServerState where
  parseJSON = withObject "ServerState" $ \o ->
    ServerState
      <$> o .:? "dl_info_speed"
      <*> o .:? "up_info_speed"
      <*> o .:? "dl_info_data"
      <*> o .:? "up_info_data"

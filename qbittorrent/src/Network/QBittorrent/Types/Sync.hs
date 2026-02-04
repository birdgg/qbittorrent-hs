-- | Sync API types for qBittorrent
module Network.QBittorrent.Types.Sync
  ( SyncMainData (..)
  , SyncTorrentInfo (..)
  , ServerState (..)
  , CategoryInfo (..)
  , SyncTorrentPeers (..)
  , PeerInfo (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Torrent (TorrentState)

-- | Category information
data CategoryInfo = CategoryInfo
  { name :: Maybe Text
  , savePath :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CategoryInfo where
  parseJSON = withObject "CategoryInfo" $ \o ->
    CategoryInfo
      <$> o .:? "name"
      <*> o .:? "savePath"

-- | Sync maindata response from qBittorrent
data SyncMainData = SyncMainData
  { rid :: Int64
  , fullUpdate :: Bool
  , torrents :: Map Text SyncTorrentInfo
  , torrentsRemoved :: [Text]
  , categories :: Map Text CategoryInfo
  , categoriesRemoved :: [Text]
  , tags :: [Text]
  , tagsRemoved :: [Text]
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
      <*> o .:? "categories" .!= mempty
      <*> o .:? "categories_removed" .!= []
      <*> o .:? "tags" .!= []
      <*> o .:? "tags_removed" .!= []
      <*> o .:? "server_state"

-- | Partial torrent info for sync API
data SyncTorrentInfo = SyncTorrentInfo
  { name :: Maybe Text
  , state :: Maybe TorrentState
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

-- | Sync torrent peers response
data SyncTorrentPeers = SyncTorrentPeers
  { rid :: Int64
  , fullUpdate :: Maybe Bool
  , peers :: Map Text PeerInfo
  , peersRemoved :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SyncTorrentPeers where
  parseJSON = withObject "SyncTorrentPeers" $ \o ->
    SyncTorrentPeers
      <$> o .: "rid"
      <*> o .:? "full_update"
      <*> o .:? "peers" .!= mempty
      <*> o .:? "peers_removed" .!= []

-- | Peer information
data PeerInfo = PeerInfo
  { ip :: Maybe Text
  , port :: Maybe Int
  , client :: Maybe Text
  , progress :: Maybe Double
  , dlSpeed :: Maybe Int64
  , upSpeed :: Maybe Int64
  , downloaded :: Maybe Int64
  , uploaded :: Maybe Int64
  , connection :: Maybe Text
  , flags :: Maybe Text
  , flagsDesc :: Maybe Text
  , relevance :: Maybe Double
  , country :: Maybe Text
  , countryCode :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PeerInfo where
  parseJSON = withObject "PeerInfo" $ \o ->
    PeerInfo
      <$> o .:? "ip"
      <*> o .:? "port"
      <*> o .:? "client"
      <*> o .:? "progress"
      <*> o .:? "dl_speed"
      <*> o .:? "up_speed"
      <*> o .:? "downloaded"
      <*> o .:? "uploaded"
      <*> o .:? "connection"
      <*> o .:? "flags"
      <*> o .:? "flags_desc"
      <*> o .:? "relevance"
      <*> o .:? "country"
      <*> o .:? "country_code"

-- | Torrent-related types for qBittorrent API
module Network.QBittorrent.Types.Torrent
  ( -- * Torrent Info
    TorrentInfo (..)
  , isCompleted

    -- * Torrent Files
  , TorrentFile (..)

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

import Data.HashMap.Strict qualified as HashMap

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
    || t.state == "pausedUP"
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
  , paused :: Maybe Bool
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
        , ("paused" .=) <$> req.paused
        ]

instance ToForm AddTorrentRequest where
  toForm req =
    Form $
      HashMap.fromList $
        catMaybes
          [ fmap (\v -> ("urls", [v])) req.urls
          , fmap (\v -> ("savepath", [v])) req.savepath
          , fmap (\v -> ("category", [v])) req.category
          , fmap (\v -> ("tags", [v])) req.tags
          , fmap (\v -> ("rename", [v])) req.rename
          , fmap (\v -> ("paused", [if v then "true" else "false"])) req.paused
          ]

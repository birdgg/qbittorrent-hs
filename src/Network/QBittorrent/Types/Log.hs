-- | Log types for qBittorrent API
module Network.QBittorrent.Types.Log
  ( -- * Log Entry
    LogEntry (..)
  , LogType (..)

    -- * Peer Log Entry
  , PeerLogEntry (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Log message type
--
-- Values are based on qBittorrent log type constants:
-- 1 = Normal, 2 = Info, 4 = Warning, 8 = Critical
data LogType
  = LogNormal    -- 1
  | LogInfo      -- 2
  | LogWarning   -- 4
  | LogCritical  -- 8
  deriving stock (Show, Eq, Generic)

instance FromJSON LogType where
  parseJSON = withScientific "LogType" $ \n ->
    case round n :: Int of
      1 -> pure LogNormal
      2 -> pure LogInfo
      4 -> pure LogWarning
      8 -> pure LogCritical
      x -> fail $ "Unknown log type: " <> show x

-- | Log entry from qBittorrent
data LogEntry = LogEntry
  { id :: Int64
  , message :: Text
  , timestamp :: Int64
  , type_ :: LogType
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \o ->
    LogEntry
      <$> o .: "id"
      <*> o .: "message"
      <*> o .: "timestamp"
      <*> o .: "type"

-- | Peer log entry from qBittorrent
data PeerLogEntry = PeerLogEntry
  { id :: Int64
  , ip :: Text
  , timestamp :: Int64
  , blocked :: Bool
  , reason :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PeerLogEntry where
  parseJSON = withObject "PeerLogEntry" $ \o ->
    PeerLogEntry
      <$> o .: "id"
      <*> o .: "ip"
      <*> o .: "timestamp"
      <*> o .: "blocked"
      <*> o .: "reason"

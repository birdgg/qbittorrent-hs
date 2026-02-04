-- | Transfer types for qBittorrent API
module Network.QBittorrent.Types.Transfer
  ( TransferInfo (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Global transfer info
data TransferInfo = TransferInfo
  { dlInfoSpeed :: Int64         -- ^ Current global download speed (bytes/s)
  , dlInfoData :: Int64          -- ^ Data downloaded this session (bytes)
  , upInfoSpeed :: Int64         -- ^ Current global upload speed (bytes/s)
  , upInfoData :: Int64          -- ^ Data uploaded this session (bytes)
  , dlRateLimit :: Int64         -- ^ Download rate limit (bytes/s, 0 = unlimited)
  , upRateLimit :: Int64         -- ^ Upload rate limit (bytes/s, 0 = unlimited)
  , dhtNodes :: Int64            -- ^ DHT nodes connected to
  , connectionStatus :: Text     -- ^ Connection status: "connected", "firewalled", "disconnected"
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TransferInfo where
  parseJSON = withObject "TransferInfo" $ \o ->
    TransferInfo
      <$> o .: "dl_info_speed"
      <*> o .: "dl_info_data"
      <*> o .: "up_info_speed"
      <*> o .: "up_info_data"
      <*> o .: "dl_rate_limit"
      <*> o .: "up_rate_limit"
      <*> o .: "dht_nodes"
      <*> o .: "connection_status"

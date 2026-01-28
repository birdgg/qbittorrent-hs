-- | App types for qBittorrent API
module Network.QBittorrent.Types.App
  ( BuildInfo (..)
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
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

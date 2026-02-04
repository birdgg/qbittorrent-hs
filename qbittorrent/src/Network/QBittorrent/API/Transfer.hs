-- | Transfer routes for qBittorrent Web API
module Network.QBittorrent.API.Transfer
  ( TransferRoutes (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Form (BanPeersForm, TransferLimitForm)
import Network.QBittorrent.Types.Transfer (TransferInfo)
import Servant.API

-- | Transfer routes
data TransferRoutes mode = TransferRoutes
  { -- | Get global transfer info
    info
      :: mode
        :- "info"
          :> Get '[JSON] TransferInfo
  , -- | Get alternative speed limits state
    --
    -- Returns "1" if enabled, "0" if disabled
    speedLimitsMode
      :: mode
        :- "speedLimitsMode"
          :> Get '[PlainText] Text
  , -- | Toggle alternative speed limits
    toggleSpeedLimitsMode
      :: mode
        :- "toggleSpeedLimitsMode"
          :> Post '[PlainText] NoContent
  , -- | Get global download limit (bytes/s, 0 means unlimited)
    downloadLimit
      :: mode
        :- "downloadLimit"
          :> Get '[PlainText] Text
  , -- | Set global download limit (bytes/s, 0 means unlimited)
    setDownloadLimit
      :: mode
        :- "setDownloadLimit"
          :> ReqBody '[FormUrlEncoded] TransferLimitForm
          :> Post '[PlainText] NoContent
  , -- | Get global upload limit (bytes/s, 0 means unlimited)
    uploadLimit
      :: mode
        :- "uploadLimit"
          :> Get '[PlainText] Text
  , -- | Set global upload limit (bytes/s, 0 means unlimited)
    setUploadLimit
      :: mode
        :- "setUploadLimit"
          :> ReqBody '[FormUrlEncoded] TransferLimitForm
          :> Post '[PlainText] NoContent
  , -- | Ban peers
    --
    -- Peers should be in format "host:port", separated by newlines
    banPeers
      :: mode
        :- "banPeers"
          :> ReqBody '[FormUrlEncoded] BanPeersForm
          :> Post '[PlainText] NoContent
  }
  deriving stock (Generic)

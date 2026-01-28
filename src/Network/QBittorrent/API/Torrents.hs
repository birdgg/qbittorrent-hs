-- | Torrents routes for qBittorrent Web API
module Network.QBittorrent.API.Torrents
  ( TorrentsRoutes (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Form
import Network.QBittorrent.Types.Torrent (AddTorrentRequest, TorrentFile, TorrentInfo)
import Servant.API

-- | Torrents routes
data TorrentsRoutes mode = TorrentsRoutes
  { add
      :: mode
        :- "add"
          :> ReqBody '[FormUrlEncoded] AddTorrentRequest
          :> Post '[PlainText] Text
  , info
      :: mode
        :- "info"
          :> QueryParam "filter" Text
          :> QueryParam "category" Text
          :> QueryParam "tag" Text
          :> QueryParam "hashes" Text
          :> Get '[JSON] [TorrentInfo]
  , files
      :: mode
        :- "files"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] [TorrentFile]
  , pause
      :: mode
        :- "pause"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , resume
      :: mode
        :- "resume"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , delete
      :: mode
        :- "delete"
          :> ReqBody '[FormUrlEncoded] DeleteTorrentsForm
          :> Post '[PlainText] NoContent
  , addTags
      :: mode
        :- "addTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent
  , removeTags
      :: mode
        :- "removeTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent
  , renameFile
      :: mode
        :- "renameFile"
          :> ReqBody '[FormUrlEncoded] RenameFileForm
          :> Post '[PlainText] NoContent
  , renameFolder
      :: mode
        :- "renameFolder"
          :> ReqBody '[FormUrlEncoded] RenameFolderForm
          :> Post '[PlainText] NoContent
  , setLocation
      :: mode
        :- "setLocation"
          :> ReqBody '[FormUrlEncoded] SetLocationForm
          :> Post '[PlainText] NoContent
  }
  deriving stock (Generic)

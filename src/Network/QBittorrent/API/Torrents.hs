-- | Torrents routes for qBittorrent Web API
module Network.QBittorrent.API.Torrents
  ( TorrentsRoutes (..)
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.QBittorrent.Types.Form
import Network.QBittorrent.Types.Torrent
import Servant.API

-- | Torrents routes
data TorrentsRoutes mode = TorrentsRoutes
  { -- Basic operations
    add
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
  , stop
      :: mode
        :- "stop"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , start
      :: mode
        :- "start"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , delete
      :: mode
        :- "delete"
          :> ReqBody '[FormUrlEncoded] DeleteTorrentsForm
          :> Post '[PlainText] NoContent
  , -- Query endpoints
    properties
      :: mode
        :- "properties"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] TorrentProperties
  , trackers
      :: mode
        :- "trackers"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] [TorrentTracker]
  , webseeds
      :: mode
        :- "webseeds"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] [TorrentWebSeed]
  , pieceStates
      :: mode
        :- "pieceStates"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] [Int]
  , pieceHashes
      :: mode
        :- "pieceHashes"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[JSON] [Text]
  , categories
      :: mode
        :- "categories"
          :> Get '[JSON] (Map Text Category)
  , tags
      :: mode
        :- "tags"
          :> Get '[JSON] [Text]
  , export
      :: mode
        :- "export"
          :> QueryParam' '[Required, Strict] "hash" Text
          :> Get '[OctetStream] ByteString
  , -- Priority management
    recheck
      :: mode
        :- "recheck"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , reannounce
      :: mode
        :- "reannounce"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , increasePrio
      :: mode
        :- "increasePrio"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , decreasePrio
      :: mode
        :- "decreasePrio"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , topPrio
      :: mode
        :- "topPrio"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , bottomPrio
      :: mode
        :- "bottomPrio"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , -- Limit settings
    setFilePrio
      :: mode
        :- "setFilePrio"
          :> ReqBody '[FormUrlEncoded] FilePrioForm
          :> Post '[PlainText] NoContent
  , setDownloadLimit
      :: mode
        :- "setDownloadLimit"
          :> ReqBody '[FormUrlEncoded] LimitForm
          :> Post '[PlainText] NoContent
  , setUploadLimit
      :: mode
        :- "setUploadLimit"
          :> ReqBody '[FormUrlEncoded] LimitForm
          :> Post '[PlainText] NoContent
  , setShareLimits
      :: mode
        :- "setShareLimits"
          :> ReqBody '[FormUrlEncoded] ShareLimitsForm
          :> Post '[PlainText] NoContent
  , -- Behavior settings
    setSuperSeeding
      :: mode
        :- "setSuperSeeding"
          :> ReqBody '[FormUrlEncoded] BoolForm
          :> Post '[PlainText] NoContent
  , setForceStart
      :: mode
        :- "setForceStart"
          :> ReqBody '[FormUrlEncoded] BoolForm
          :> Post '[PlainText] NoContent
  , setAutoManagement
      :: mode
        :- "setAutoManagement"
          :> ReqBody '[FormUrlEncoded] BoolForm
          :> Post '[PlainText] NoContent
  , toggleSequentialDownload
      :: mode
        :- "toggleSequentialDownload"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , toggleFirstLastPiecePrio
      :: mode
        :- "toggleFirstLastPiecePrio"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent
  , -- Category management
    setCategory
      :: mode
        :- "setCategory"
          :> ReqBody '[FormUrlEncoded] CategoryForm
          :> Post '[PlainText] NoContent
  , createCategory
      :: mode
        :- "createCategory"
          :> ReqBody '[FormUrlEncoded] CreateCategoryForm
          :> Post '[PlainText] NoContent
  , editCategory
      :: mode
        :- "editCategory"
          :> ReqBody '[FormUrlEncoded] CreateCategoryForm
          :> Post '[PlainText] NoContent
  , removeCategories
      :: mode
        :- "removeCategories"
          :> ReqBody '[FormUrlEncoded] CategoriesForm
          :> Post '[PlainText] NoContent
  , -- Tag management
    addTags
      :: mode
        :- "addTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent
  , removeTags
      :: mode
        :- "removeTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent
  , createTags
      :: mode
        :- "createTags"
          :> ReqBody '[FormUrlEncoded] TagsOnlyForm
          :> Post '[PlainText] NoContent
  , deleteTags
      :: mode
        :- "deleteTags"
          :> ReqBody '[FormUrlEncoded] TagsOnlyForm
          :> Post '[PlainText] NoContent
  , -- Tracker management
    addTrackers
      :: mode
        :- "addTrackers"
          :> ReqBody '[FormUrlEncoded] AddTrackersForm
          :> Post '[PlainText] NoContent
  , editTracker
      :: mode
        :- "editTracker"
          :> ReqBody '[FormUrlEncoded] EditTrackerForm
          :> Post '[PlainText] NoContent
  , removeTrackers
      :: mode
        :- "removeTrackers"
          :> ReqBody '[FormUrlEncoded] RemoveTrackersForm
          :> Post '[PlainText] NoContent
  , addPeers
      :: mode
        :- "addPeers"
          :> ReqBody '[FormUrlEncoded] AddPeersForm
          :> Post '[PlainText] NoContent
  , -- Rename operations
    rename
      :: mode
        :- "rename"
          :> ReqBody '[FormUrlEncoded] RenameForm
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

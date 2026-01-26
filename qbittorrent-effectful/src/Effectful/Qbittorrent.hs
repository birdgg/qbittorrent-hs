{- | Effectful bindings for qBittorrent client

This module provides an effectful Effect for qBittorrent operations with
automatic session management.

= Features

- Automatic login on first operation
- Cookie persistence via servant-client's CookieJar
- Auto-retry on auth failure

= Usage

@
import Effectful
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Effectful.Qbittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let config = defaultConfig
        { host = "localhost"
        , port = 8080
        }

  runEff
    . runQBittorrent manager config
    $ do
      torrents <- qbGetTorrents Nothing
      liftIO $ print torrents
@
-}
module Effectful.Qbittorrent
  ( -- * Effect
    QBittorrent (..)

    -- * Handlers
  , runQBittorrent

    -- * Operations
  , qbLogin
  , qbLogout
  , qbAddTorrent
  , qbGetTorrents
  , qbGetTorrentFiles
  , qbPauseTorrents
  , qbResumeTorrents
  , qbDeleteTorrents
  , qbAddTags
  , qbRemoveTags
  , qbRenameFile
  , qbRenameFolder
  , qbSetLocation
  , qbGetDefaultSavePath
  , qbSetPreferences
  , qbSyncMaindata

    -- * Re-exports
  , module Network.QBittorrent.Types
  ) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Network.HTTP.Client (Manager)
import Network.QBittorrent.Client qualified as Client
import Network.QBittorrent.Types
import Servant.API (NoContent)
import Servant.Client (ClientEnv, runClientM)
import Servant.Client qualified as Servant

-- | QBittorrent effect
data QBittorrent :: Effect where
  QbLogin :: QBittorrent m Text
  QbLogout :: QBittorrent m NoContent
  QbAddTorrent :: AddTorrentRequest -> QBittorrent m Text
  QbGetTorrents :: Maybe TorrentInfoRequest -> QBittorrent m [TorrentInfo]
  QbGetTorrentFiles :: Text -> QBittorrent m [TorrentFile]
  QbPauseTorrents :: [Text] -> QBittorrent m NoContent
  QbResumeTorrents :: [Text] -> QBittorrent m NoContent
  QbDeleteTorrents :: [Text] -> Bool -> QBittorrent m NoContent
  QbAddTags :: [Text] -> [Text] -> QBittorrent m NoContent
  QbRemoveTags :: [Text] -> [Text] -> QBittorrent m NoContent
  QbRenameFile :: Text -> Text -> Text -> QBittorrent m NoContent
  QbRenameFolder :: Text -> Text -> Text -> QBittorrent m NoContent
  QbSetLocation :: [Text] -> Text -> QBittorrent m NoContent
  QbGetDefaultSavePath :: QBittorrent m Text
  QbSetPreferences :: Aeson.Value -> QBittorrent m NoContent
  QbSyncMaindata :: Int64 -> QBittorrent m SyncMainData

type instance DispatchOf QBittorrent = Dynamic

makeEffect ''QBittorrent

-- | Session state for auto-auth
newtype SessionState = SessionState
  { isLoggedIn :: Bool
  }

newSessionState :: SessionState
newSessionState = SessionState{isLoggedIn = False}

-- | Run QBittorrent effect with automatic session management
runQBittorrent ::
  (IOE :> es) =>
  Manager ->
  QBConfig ->
  Eff (QBittorrent : es) a ->
  Eff es a
runQBittorrent manager config action = do
  cookieJarVar <- liftIO $ newTVarIO mempty
  let baseEnv = Servant.mkClientEnv manager (Client.mkBaseUrl config)
      clientEnv = baseEnv{Servant.cookieJar = Just cookieJarVar}
  sessionRef <- liftIO $ newIORef newSessionState
  interpret (handleQBittorrent clientEnv config sessionRef) action

-- | Effect handler
handleQBittorrent ::
  (IOE :> es) =>
  ClientEnv ->
  QBConfig ->
  IORef SessionState ->
  EffectHandler QBittorrent es
handleQBittorrent env config sessionRef _ = \case
  QbLogin -> runWithAuth env config sessionRef $ Client.login config
  QbLogout -> runWithAuth env config sessionRef Client.logout
  QbAddTorrent req -> runWithAuth env config sessionRef $ Client.addTorrent req
  QbGetTorrents mReq -> runWithAuth env config sessionRef $ Client.getTorrents mReq
  QbGetTorrentFiles hash -> runWithAuth env config sessionRef $ Client.getTorrentFiles hash
  QbPauseTorrents hashes -> runWithAuth env config sessionRef $ Client.pauseTorrents hashes
  QbResumeTorrents hashes -> runWithAuth env config sessionRef $ Client.resumeTorrents hashes
  QbDeleteTorrents hashes del -> runWithAuth env config sessionRef $ Client.deleteTorrents hashes del
  QbAddTags hashes tags -> runWithAuth env config sessionRef $ Client.addTags hashes tags
  QbRemoveTags hashes tags -> runWithAuth env config sessionRef $ Client.removeTags hashes tags
  QbRenameFile hash old new -> runWithAuth env config sessionRef $ Client.renameFile hash old new
  QbRenameFolder hash old new -> runWithAuth env config sessionRef $ Client.renameFolder hash old new
  QbSetLocation hashes loc -> runWithAuth env config sessionRef $ Client.setLocation hashes loc
  QbGetDefaultSavePath -> runWithAuth env config sessionRef Client.getDefaultSavePath
  QbSetPreferences prefs -> runWithAuth env config sessionRef $ Client.setPreferences prefs
  QbSyncMaindata rid -> runWithAuth env config sessionRef $ Client.syncMaindata rid

-- | Run operation with auto-auth
runWithAuth ::
  (IOE :> es) =>
  ClientEnv ->
  QBConfig ->
  IORef SessionState ->
  Client.ClientM a ->
  Eff es a
runWithAuth env config sessionRef action = do
  ensureLoggedIn env config sessionRef
  result <- liftIO $ runClientM action env
  case result of
    Right a -> pure a
    Left err -> do
      -- Try re-login once on auth error
      case err of
        Servant.FailureResponse _ resp
          | isAuthError resp -> do
              doLogin env config sessionRef
              retryResult <- liftIO $ runClientM action env
              case retryResult of
                Right a -> pure a
                Left err' -> error $ "QBittorrent error: " <> show err'
        _ -> error $ "QBittorrent error: " <> show err

isAuthError :: Servant.Response -> Bool
isAuthError resp =
  let code = Servant.responseStatusCode resp
   in code == toEnum 401 || code == toEnum 403

-- | Ensure we're logged in
ensureLoggedIn ::
  (IOE :> es) =>
  ClientEnv ->
  QBConfig ->
  IORef SessionState ->
  Eff es ()
ensureLoggedIn env config sessionRef = do
  session <- liftIO $ readIORef sessionRef
  if session.isLoggedIn
    then pure ()
    else doLogin env config sessionRef

-- | Perform login
doLogin ::
  (IOE :> es) =>
  ClientEnv ->
  QBConfig ->
  IORef SessionState ->
  Eff es ()
doLogin env config sessionRef = do
  loginResult <- liftIO $ runClientM (Client.login config) env
  case loginResult of
    Right resp
      | resp == "Ok." -> liftIO $ writeIORef sessionRef SessionState{isLoggedIn = True}
      | otherwise -> error $ "QBittorrent login failed: " <> show resp
    Left err -> error $ "QBittorrent login error: " <> show err

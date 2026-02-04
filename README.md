# qbittorrent-hs

Haskell client library for the qBittorrent Web API.

**Requires qBittorrent 5.0 or later.** This library does not support qBittorrent 4.x.

## Packages

This repository contains two packages:

| Package | Description |
|---------|-------------|
| [`qbittorrent`](./qbittorrent/) | Core library with servant-client bindings |
| [`effectful-qbittorrent`](./effectful-qbittorrent/) | [Effectful](https://hackage.haskell.org/package/effectful) integration |

## Installation

```cabal
-- Core library
build-depends: qbittorrent

-- With Effectful support
build-depends: qbittorrent, effectful-qbittorrent
```

## Quick Start

### Simple Client (Recommended)

The simplest way to use the library. Provides a record-based API with `IO (Either QBError a)` return types and automatic session management:

```haskell
import Network.QBittorrent.SimpleClient qualified as QB

main :: IO ()
main = do
  result <- QB.newClient QB.defaultConfig
  case result of
    Left err -> print err
    Right client -> do
      -- Use record fields directly
      torrents <- client.getTorrents Nothing
      print torrents

      version <- client.getVersion
      print version
```

### With ReaderT (Recommended for Applications)

For larger applications, use `ReaderT` to pass the client through your app:

```haskell
import Control.Monad.Reader
import Network.QBittorrent.SimpleClient qualified as QB

type App a = ReaderT QB.Client IO a

runApp :: QB.QBConfig -> App a -> IO a
runApp config action = do
  Right client <- QB.newClient config
  runReaderT action client

myApp :: App ()
myApp = do
  client <- ask
  liftIO $ do
    torrents <- client.getTorrents Nothing
    print torrents

main :: IO ()
main = runApp QB.defaultConfig myApp
```

### Advanced Usage (ClientM)

For more control or to compose multiple API calls in `ClientM`:

```haskell
import Network.QBittorrent

main :: IO ()
main = do
  let config = defaultConfig
        { host = "localhost"
        , port = 8080
        , username = "admin"
        , password = "adminadmin"
        }
  client <- newClient config

  -- Login
  result <- runQB client (login config)
  case result of
    Right "Ok." -> putStrLn "Logged in!"
    _ -> putStrLn "Login failed"

  -- Get torrents (session cookie is managed automatically)
  torrents <- runQB client (getTorrents Nothing)
  print torrents
```

For custom HTTP manager settings, use `newClientWith`:

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  client <- newClientWith manager config
  -- ...
```

### Multiple Clients with Shared Manager

When connecting to multiple qBittorrent instances, share the HTTP manager for better performance:

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent.SimpleClient qualified as QB

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  Right client1 <- QB.newClientWith manager config1
  Right client2 <- QB.newClientWith manager config2

  -- Both clients share the same connection pool
  torrents1 <- client1.getTorrents Nothing
  torrents2 <- client2.getTorrents Nothing
  print (torrents1, torrents2)
```

### With Effectful

Use the `effectful-qbittorrent` package for effectful integration. Errors are handled via the `Error QBError` effect, enabling clean do-notation without explicit `Either` handling:

```haskell
import Effectful
import Effectful.QBittorrent

main :: IO ()
main = do
  client <- newClient defaultConfig

  result <- runEff . runErrorNoCallStack @QBError . runQBittorrent client $ do
    login defaultConfig        -- Returns Text, throws on failure
    getTorrents Nothing        -- Returns [TorrentInfo]

  case result of
    Left err -> print err
    Right torrents -> print torrents
```

For more control over error handling, use `catchError`:

```haskell
myApp :: (QBittorrent :> es, Error QBError :> es, IOE :> es) => Eff es [TorrentInfo]
myApp = do
  login defaultConfig `catchError` \_ err ->
    case err of
      AuthError _ -> login fallbackConfig  -- Retry with fallback
      _           -> throwError err        -- Re-throw other errors
  getTorrents Nothing
```

## API Coverage

### Authentication API ✅ (2/2)

| Endpoint | Status |
|----------|--------|
| `login` | ✅ |
| `logout` | ✅ |

### Application API ✅ (9/9)

| Endpoint | Status |
|----------|--------|
| `version` | ✅ |
| `webapiVersion` | ✅ |
| `buildInfo` | ✅ |
| `shutdown` | ✅ |
| `preferences` | ✅ |
| `setPreferences` | ✅ |
| `defaultSavePath` | ✅ |
| `networkInterfaceList` | ✅ |
| `networkInterfaceAddressList` | ✅ |

### Torrents API ✅ (47/47)

| Endpoint | Status |
|----------|--------|
| `info` | ✅ |
| `properties` | ✅ |
| `trackers` | ✅ |
| `webseeds` | ✅ |
| `files` | ✅ |
| `pieceStates` | ✅ |
| `pieceHashes` | ✅ |
| `add` | ✅ |
| `stop` | ✅ |
| `start` | ✅ |
| `delete` | ✅ |
| `recheck` | ✅ |
| `reannounce` | ✅ |
| `increasePrio` | ✅ |
| `decreasePrio` | ✅ |
| `topPrio` | ✅ |
| `bottomPrio` | ✅ |
| `setFilePrio` | ✅ |
| `setDownloadLimit` | ✅ |
| `setUploadLimit` | ✅ |
| `setShareLimits` | ✅ |
| `setSuperSeeding` | ✅ |
| `setForceStart` | ✅ |
| `setAutoManagement` | ✅ |
| `toggleSequentialDownload` | ✅ |
| `toggleFirstLastPiecePrio` | ✅ |
| `setCategory` | ✅ |
| `categories` | ✅ |
| `createCategory` | ✅ |
| `editCategory` | ✅ |
| `removeCategories` | ✅ |
| `tags` | ✅ |
| `addTags` | ✅ |
| `removeTags` | ✅ |
| `createTags` | ✅ |
| `deleteTags` | ✅ |
| `addTrackers` | ✅ |
| `editTracker` | ✅ |
| `removeTrackers` | ✅ |
| `addPeers` | ✅ |
| `rename` | ✅ |
| `renameFile` | ✅ |
| `renameFolder` | ✅ |
| `setLocation` | ✅ |
| `export` | ✅ |
| `count` | ✅ |
| `downloadLimit` (get) | ✅ |
| `uploadLimit` (get) | ✅ |

### Sync API ✅ (2/2)

| Endpoint | Status |
|----------|--------|
| `maindata` | ✅ |
| `torrentPeers` | ✅ |

### Log API ✅ (2/2)

| Endpoint | Status |
|----------|--------|
| `main` | ✅ |
| `peers` | ✅ |

### Transfer API ✅ (8/8)

| Endpoint | Status |
|----------|--------|
| `info` | ✅ |
| `speedLimitsMode` | ✅ |
| `toggleSpeedLimitsMode` | ✅ |
| `downloadLimit` | ✅ |
| `setDownloadLimit` | ✅ |
| `uploadLimit` | ✅ |
| `setUploadLimit` | ✅ |
| `banPeers` | ✅ |

### RSS API ❌

Not implemented.

### Search API ❌

Not implemented.

## License

MIT
